module Option = BatOption

module type TABLE = Ocsipersist_lib.Sigs.TABLE

module Config = struct
  let secure = ref true
  let region = ref None
  let credentials = Bs_aws.Credentials.get_defaults ()
  let table_prefix = ref None
end

module Registration = struct
  let parse_config = function
    | [] -> ()
    | [Xml.Element ("aws", attrs, [])] ->
        let parse_attr = function
          | "secure", s -> Config.secure := bool_of_string s
          | "region", r -> Config.region := Some r
          | "table-prefix", r -> Config.table_prefix := Some r
          | _ ->
              raise
              @@ Ocsigen_extensions.Error_in_config_file
                   "Unexpected attribute for <database> in Ocsipersist config"
        in
        List.iter parse_attr attrs
    | _ ->
        raise
        @@ Ocsigen_extensions.Error_in_config_file
             "Unexpected content inside Ocsipersist config"

  let _ =
    Ocsigen_extensions.register ~name:"ocsipersist" ~init_fun:parse_config ()
end

module AwsConf : Bs_aws.Service.CONF = struct
  let secure = !Config.secure

  let region =
    match !Config.region with
    | None -> failwith "no region defined"
    | Some r -> Bs_aws.Common.Region.from_string r

  let credentials = Config.credentials
end

module Dynamodb = Bs_aws.Dynamodb.Make (AwsConf)

module Aux = struct
  let table n = Option.map_default (fun p -> p ^ "-" ^ n) n !Config.table_prefix

  let create_table ~attributes ~primary_key ?sort_key table =
    try%lwt
      Lwt.map ignore
      @@ Dynamodb.create_table ~attributes ~primary_key ?sort_key table
    with Dynamodb.ResourceInUse _ -> Lwt.return_unit

  let get_item ~decode ~table ~vkey key =
    let%lwt response =
      Dynamodb.get_item ~table ~projection_expression:vkey key
    in
    match response.Dynamodb.GetItem.item with
    | None -> Lwt.fail Not_found
    | Some [(key, value)] ->
        assert (key = vkey);
        Lwt.return @@ decode value
    | Some l ->
        failwith @@ __LOC__ ^ ": unexpected number of attributes: "
        ^ String.concat ", " (List.map fst l)
end

module Store = struct
  type store = string
  type 'a t = {store : string; name : string}

  let kkey = "k"
  let vkey = "val"

  let open_store store =
    let%lwt () =
      Aux.create_table
        ~attributes:[kkey, `B]
        ~primary_key:kkey (Aux.table store)
    in
    Lwt.return store

  let encode v = Dynamodb.B (Marshal.to_string v [])

  let decode = function
    | Dynamodb.B b -> Marshal.from_string b 0
    | _ -> failwith "value should be of type binary"

  let make_persistent ~store ~name ~default =
    let condition_expression =
      Dynamodb.ConditionExpression.(Function (Attribute_not_exists kkey))
    in
    let%lwt _ =
      Dynamodb.put_item ~table:(Aux.table store) ~condition_expression
        [kkey, B name; vkey, encode default]
    in
    Lwt.return {store; name}

  let make_persistent_lazy_lwt ~store ~name ~default =
    try%lwt
      let%lwt _ =
        Aux.get_item ~decode ~table:(Aux.table store) ~vkey [kkey, B name]
      in
      Lwt.return {store; name}
    with Not_found ->
      let%lwt default = default () in
      make_persistent ~store ~name ~default

  let make_persistent_lazy ~store ~name ~default =
    let default () = Lwt.return @@ default () in
    make_persistent_lazy_lwt ~store ~name ~default

  let get {name; store} =
    Aux.get_item ~decode ~table:(Aux.table store) ~vkey [kkey, B name]

  let set {store; name} value =
    Lwt.map ignore
    @@ Dynamodb.put_item ~table:(Aux.table store)
         [kkey, B name; vkey, encode value]
end

type store = Store.store
type 'a variable = 'a Store.t

module Functorial = struct
  type internal = Dynamodb.attribute_value

  module type COLUMN = sig
    type t

    val column_type : string
    val encode : t -> internal
    val decode : internal -> t
  end

  module Table (T : sig
    val name : string
  end)
  (Key : COLUMN)
  (Value : COLUMN) : TABLE with type key = Key.t and type value = Value.t =
  struct
    type key = Key.t
    type value = Value.t

    let kkey = "k"
    let vkey = "val"
    let name = T.name

    let column_type_of_string = function
      | "binary" | "b" | "Binary" | "B" -> `B
      | "number" | "n" | "Number" | "N" -> `N
      | "string" | "s" | "String" | "S" -> `S
      | s -> failwith @@ "unsupported column type: " ^ s

    let init =
      Aux.create_table
        ~attributes:[kkey, column_type_of_string @@ Key.column_type]
        ~primary_key:kkey (Aux.table T.name)

    let with_table f = Lwt.bind init (fun () -> f @@ Aux.table T.name)

    let find k =
      with_table @@ fun table ->
      Aux.get_item ~decode:Value.decode ~table ~vkey [kkey, Key.encode k]

    let add k v =
      with_table @@ fun table ->
      Lwt.map ignore
      @@ Dynamodb.put_item ~table [kkey, Key.encode k; vkey, Value.encode v]

    let replace_if_exists k v =
      with_table @@ fun table ->
      let condition_expression =
        Dynamodb.ConditionExpression.(Function (Attribute_exists kkey))
      in
      Lwt.map ignore
      @@ Dynamodb.put_item ~table ~condition_expression
           [kkey, Key.encode k; vkey, Value.encode v]

    let remove k =
      with_table @@ fun table ->
      Lwt.map ignore @@ Dynamodb.delete_item ~table [kkey, Key.encode k]

    let modify_opt k f =
      let%lwt old_value =
        try%lwt Lwt.map Option.some (find k) with Not_found -> Lwt.return_none
      in
      let new_value = f old_value in
      if old_value = new_value
      then Lwt.return_unit
      else match new_value with None -> remove k | Some v -> add k v

    let length () =
      with_table @@ fun table ->
      let open Dynamodb in
      let%lwt response = describe_table table in
      Lwt.return (Option.default 0 @@ DescribeTable.(response.table.item_count))

    let rec iter_rec ~table ?exclusive_start_key ?limit ?lt ?leq f =
      let projection_expression = kkey ^ ", " ^ vkey in
      let%lwt response =
        Dynamodb.scan ?exclusive_start_key ?limit ~projection_expression ~table
          ()
      in
      let completed = ref false in
      let rec handle_items = function
        | [] -> Lwt.return_unit
        | h :: t ->
            let k, v =
              try
                ( Key.decode @@ List.assoc kkey h
                , Value.decode @@ List.assoc vkey h )
              with Not_found -> failwith __LOC__
            in
            completed :=
              Option.map_default (fun lt -> k >= lt) false lt
              || Option.map_default (fun leq -> k > leq) false leq;
            if !completed
            then Lwt.return_unit
            else
              let%lwt () = f k v in
              handle_items t
      in
      let items = response.Dynamodb.Scan.items in
      let%lwt () = handle_items items in
      match response.Dynamodb.Scan.last_evaluated_key with
      | None -> Lwt.return_unit
      | Some last_evaluated_key ->
          let limit =
            Option.map
              (fun l -> l - List.length response.Dynamodb.Scan.items)
              limit
          and exclusive_start_key = kkey, last_evaluated_key in
          iter_rec ~table ~exclusive_start_key ?limit ?lt ?leq f

    let iter ?count ?gt ?geq ?lt ?leq f =
      match geq with
      | Some _ ->
          failwith @@ __LOC__ ^ ": iter with ?geq is not supported by DynamoDB"
      | None ->
          let exclusive_start_key =
            Option.map (fun k -> kkey, Key.encode k) gt
          in
          let limit = Option.map Int64.to_int count in
          with_table @@ fun table ->
          iter_rec ~table ?exclusive_start_key ?limit ?lt ?leq f

    let iter_batch ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ =
      failwith "Ocsipersist.iter_batch not implemented for DynamoDB"

    let fold ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ = failwith __LOC__
    let iter_block ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ = failwith __LOC__

    module Variable = Ocsipersist_lib.Variable (struct
      type k = key
      type v = value

      let find = find
      let add = add
    end)
  end

  exception Type_error of string * internal

  let () =
    Printexc.register_printer @@ function
    | Type_error (expected, encountered) ->
        Some
          (Printf.sprintf "expected value of type %s but encountered %s"
             expected
             (Dynamodb.show_attribute_value encountered))
    | _ -> None

  module Column = struct
    module String = struct
      type t = string

      let column_type = "binary"
      let encode s = Dynamodb.B s

      let decode = function
        | Dynamodb.B s -> s
        | other -> raise @@ Type_error (column_type, other)
    end

    module Float = struct
      type t = float

      let column_type = "number"
      let encode f = Dynamodb.N f

      let decode = function
        | Dynamodb.N f -> f
        | other -> raise @@ Type_error (column_type, other)
    end

    module Marshal (C : sig
      type t
    end) =
    struct
      type t = C.t

      let column_type = "binary"
      let encode v = Dynamodb.B (Marshal.to_string v [])

      let decode = function
        | Dynamodb.B b -> Marshal.from_string b 0
        | other -> raise @@ Type_error (column_type, other)
    end
  end
end

module Polymorphic = Ocsipersist_lib.Polymorphic (Functorial)

type 'value table = 'value Polymorphic.table
