module Option = BatOption

module type TABLE = Ocsipersist_lib.Sigs.TABLE

module Config = struct
  let enabled = ref false
  let sync = ref false
  let secure = ref true
  let region = ref None
  let credentials = Bs_aws.Credentials.get_defaults ()
  let table_prefix = ref None
end

module Registration = struct
  let parse_tag = function
    | Xml.Element ("dynamodb", attrs, []) ->
        let parse_attr = function
          | "enabled", e -> Config.enabled := bool_of_string e
          | "sync", s -> Config.sync := bool_of_string s
          | "secure", s -> Config.secure := bool_of_string s
          | "region", r -> Config.region := Some r
          | "table-prefix", r -> Config.table_prefix := Some r
          | _ ->
              raise
              @@ Ocsigen_extensions.Error_in_config_file
                   "Unexpected attribute for <database> in Ocsipersist config"
        in
        List.iter parse_attr attrs
    | Xml.Element ("pgsql", attrs, []) ->
        Pgsql.Registration.init_fun [Xml.Element ("database", attrs, [])]
    | Xml.Element (tag, _, _) ->
        raise
        @@ Ocsigen_extensions.Error_in_config_file
             (__LOC__ ^ ": Unexpected tag inside Ocsipersist config: " ^ tag)
    | _ ->
        raise
        @@ Ocsigen_extensions.Error_in_config_file
             (__LOC__ ^ ": Unexpected content inside Ocsipersist config")

  let init_fun = List.iter parse_tag
  let _ = Ocsigen_extensions.register ~name:"ocsipersist" ~init_fun ()
end

module AwsConf : Bs_aws.Service.CONF = struct
  let secure = !Config.secure

  let region =
    match !Config.region with
    | None -> Bs_aws.Common.Region.from_string "eu-west-1"
    | Some r -> Bs_aws.Common.Region.from_string r

  let credentials = Config.credentials
end

module Dynamodb = Dynamodb.Make (AwsConf) (Config)
module D = Dynamodb
module P = Pgsql

let async f =
  if !Config.enabled then Lwt.async (fun () -> Lwt.map ignore @@ f ())

module Store = struct
  module P = P.Store
  module D = D.Store

  type store = P.store
  type 'a t = 'a P.t

  let open_store store =
    async (fun () -> D.open_store store);
    P.open_store store

  let make_persistent ~store ~name ~default =
    async (fun () -> D.make_persistent ~store ~name ~default);
    P.make_persistent ~store ~name ~default

  let make_persistent_lazy_lwt ~store ~name ~default =
    async (fun () -> D.make_persistent_lazy_lwt ~store ~name ~default);
    P.make_persistent_lazy_lwt ~store ~name ~default

  let make_persistent_lazy ~store ~name ~default =
    async (fun () -> D.make_persistent_lazy ~store ~name ~default);
    P.make_persistent_lazy ~store ~name ~default

  let get ({P.name; store} as v) =
    async (fun () ->
        try%lwt D.get {D.name; store} with Not_found -> Lwt.return_unit);
    P.get v

  let set ({P.store; name} as v) value =
    async (fun () -> D.set {D.store; name} value);
    P.set v value
end

type store = Store.store
type 'a variable = 'a Store.t

module Functorial = struct
  module P = P.Functorial
  module D = D.Functorial

  type internal = P.internal * D.internal

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

    let name = T.name

    module P =
      P.Table
        (T)
        (struct
          type t = Key.t

          let column_type = List.hd @@ String.split_on_char ',' Key.column_type
          let encode f = fst @@ Key.encode f
          let decode p = Key.decode (p, Dynamodb.Dynamodb.NULL)
        end)
        (struct
          type t = Value.t

          let column_type =
            List.hd @@ String.split_on_char ',' Value.column_type

          let encode f = fst @@ Value.encode f
          let decode p = Value.decode (p, Dynamodb.Dynamodb.NULL)
        end)

    module D =
      D.Table
        (T)
        (struct
          type t = Key.t

          let column_type =
            List.nth (String.split_on_char ',' Key.column_type) 1

          let encode f = snd @@ Key.encode f

          let decode d =
            Key.decode ("1K2trAFGQ1iu5PQyVwkIaqYrUoQdg3CxcI6yooZoCSRi2Py", d)
        end)
        (struct
          type t = Value.t

          let column_type =
            List.nth (String.split_on_char ',' Value.column_type) 1

          let encode f = snd @@ Value.encode f

          let decode d =
            Value.decode ("1K2trAFGQ1iu5PQyVwkIaqYrUoQdg3CxcI6yooZoCSRi2Py", d)
        end)

    let find key =
      async (fun () ->
          try%lwt
            let%lwt _ = D.find key in
            Lwt.return_unit
          with Not_found -> Lwt.return_unit);
      P.find key

    let add key value =
      async (fun () -> D.add key value);
      P.add key value

    let add_batch _ = failwith __LOC__

    let replace_if_exists key value =
      async (fun () -> D.replace_if_exists key value);
      P.replace_if_exists key value

    let remove key =
      async (fun () -> D.remove key);
      P.remove key

    let modify_opt key f =
      async (fun () -> D.modify_opt key f);
      P.modify_opt key f

    let length () =
      async (fun () -> D.length ());
      P.length ()

    let iter ?count ?gt ?geq ?lt ?leq f =
      async (fun () -> D.iter ?count ?gt ?geq ?lt ?leq f);
      P.iter ?count ?gt ?geq ?lt ?leq f

    let fold ?count ?gt ?geq ?lt ?leq f x =
      async (fun () -> D.fold ?count ?gt ?geq ?lt ?leq f x);
      P.fold ?count ?gt ?geq ?lt ?leq f x

    let iter_block ?count ?gt ?geq ?lt ?leq f =
      async (fun () -> D.iter_block ?count ?gt ?geq ?lt ?leq f);
      P.iter_block ?count ?gt ?geq ?lt ?leq f

    let iter_batch ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ = failwith __LOC__

    let () =
      Lwt.async @@ fun () ->
      P.iter_batch @@ fun key_values ->
      let n = List.length key_values in
      let%lwt () = if n <> 0 then D.add_batch key_values else Lwt.return_unit in
      prerr_endline @@ "synchronised " ^ string_of_int n ^ " elements";
      Lwt.return_unit

    module Variable = Ocsipersist_lib.Variable (struct
      type k = Key.t
      type v = Value.t

      let find = find
      let add = add
    end)
  end

  module Column = struct
    module P = P.Column
    module D = D.Column

    module String : COLUMN with type t = string = struct
      module P = P.String
      module D = D.String

      type t = string

      let column_type = P.column_type ^ "," ^ D.column_type
      let encode f = P.encode f, D.encode f

      let decode = function
        | p, Dynamodb.Dynamodb.NULL -> P.decode p
        | "1K2trAFGQ1iu5PQyVwkIaqYrUoQdg3CxcI6yooZoCSRi2Py", d -> D.decode d
        | _ -> assert false
    end

    module Float : COLUMN with type t = float = struct
      module P = P.Float
      module D = D.Float

      type t = float

      let column_type = P.column_type ^ "," ^ D.column_type
      let encode f = P.encode f, D.encode f

      let decode = function
        | p, Dynamodb.Dynamodb.NULL -> P.decode p
        | "1K2trAFGQ1iu5PQyVwkIaqYrUoQdg3CxcI6yooZoCSRi2Py", d -> D.decode d
        | _ -> assert false
    end

    module Marshal (C : sig
      type t
    end) : COLUMN with type t = C.t = struct
      module P = P.Marshal (C)
      module D = D.Marshal (C)

      type t = C.t

      let column_type = P.column_type ^ "," ^ D.column_type
      let encode f = P.encode f, D.encode f

      let decode = function
        | p, Dynamodb.Dynamodb.NULL -> P.decode p
        | "1K2trAFGQ1iu5PQyVwkIaqYrUoQdg3CxcI6yooZoCSRi2Py", d -> D.decode d
        | _ -> assert false
    end
  end
end

module Polymorphic = Ocsipersist_lib.Polymorphic (Functorial)

type 'value table = 'value Polymorphic.table
