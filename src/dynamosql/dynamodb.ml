module Option = BatOption

module type TABLE = Ocsipersist_lib.Sigs.TABLE

module type CONFIG = sig
  val enabled : bool ref
  val secure : bool ref
  val region : string option ref
  val credentials : Bs_aws.Common.credentials Lwt.t
  val table_prefix : string option ref
end

module Make (AwsConf : Bs_aws.Service.CONF) (Config : CONFIG) = struct
  module Dynamodb = Bs_aws.Dynamodb.Make (AwsConf)

  module Aux = struct
    let table n =
      Option.map_default (fun p -> p ^ "-" ^ n) n !Config.table_prefix

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
        lazy
          (Aux.create_table
             ~attributes:[kkey, column_type_of_string @@ Key.column_type]
             ~primary_key:kkey (Aux.table T.name))

      let with_table f =
        Lwt.bind (Lazy.force init) (fun () -> f @@ Aux.table T.name)

      let find k =
        with_table @@ fun table ->
        Aux.get_item ~decode:Value.decode ~table ~vkey [kkey, Key.encode k]

      let add_batch key_values =
        let write_requests =
          let open Dynamodb.BatchWriteItem in
          List.map
            (fun (k, v) ->
              PutRequest [kkey, Key.encode k; vkey, Value.encode v])
            key_values
        in
        with_table @@ fun table ->
        let rec loop xs =
          try
            let l, r = BatList.split_at 25 xs in
            let%lwt _ = Dynamodb.batch_write_item [table, l] in
            loop r
          with Invalid_argument _ ->
            Lwt.map ignore @@ Dynamodb.batch_write_item [table, xs]
        in
        loop write_requests

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
          try%lwt Lwt.map Option.some (find k)
          with Not_found -> Lwt.return_none
        in
        let new_value = f old_value in
        if old_value = new_value
        then Lwt.return_unit
        else match new_value with None -> remove k | Some v -> add k v

      let length () =
        with_table @@ fun table ->
        let open Dynamodb in
        let%lwt response = describe_table table in
        Lwt.return
          (Option.default 0 @@ DescribeTable.(response.table.item_count))

      let iter ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ = failwith __LOC__
      let fold ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ = failwith __LOC__
      let iter_block ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ = failwith __LOC__

      let iter_batch ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ =
        failwith "iter_batch not implemented for DynamoDB."

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
end
