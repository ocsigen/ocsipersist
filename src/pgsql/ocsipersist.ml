(** PostgreSQL (>= 9.5) backend for Ocsipersist. *)

module type TABLE = Ocsipersist_lib.Sigs.TABLE

let section = Logs.Src.create "ocsigen:ocsipersist:pgsql"

module Lwt_thread = struct
  let close_in = fun x1 -> Eio.Resource.close x1

  let really_input
      (* TODO: lwt-to-direct-style: [x2] should be a [Cstruct.t]. *)
      (* TODO: lwt-to-direct-style: [Eio.Flow.single_read] operates on a [Flow.source] but [x1] is likely of type [Eio.Buf_read.t]. Rewrite this code to use [Buf_read] (which contains an internal buffer) or change the call to [Eio.Buf_read.of_flow] used to create the buffer. *)
      (* TODO: lwt-to-direct-style: Dropped expression (buffer offset): [x3]. This will behave as if it was [0]. *)
      (* TODO: lwt-to-direct-style: Dropped expression (buffer length): [x4]. This will behave as if it was [Cstruct.length buffer]. *)
      =
   fun x1 x2 x3 x4 -> Eio.Flow.read_exact x1 x2

  let input_binary_int = Lwt_io.BE.read_int
  let input_char = Lwt_io.read_char
  let output_string = fun x1 x2 -> Eio.Buf_write.string x1 x2
  let output_binary_int = Lwt_io.BE.write_int
  let output_char = Lwt_io.write_char
  let flush = fun x1 -> Eio.Buf_write.flush x1
  let open_connection x = Lwt_io.open_connection x

  type out_channel = Eio.Buf_write.t
  type in_channel = Eio.Buf_read.t
end

module PGOCaml = PGOCaml_generic.Make (Lwt_thread)
open Printf

exception Ocsipersist_error

module Config = Ocsipersist_settings

let connect () =
  let dbhandle =
    PGOCaml.connect ?host:!Config.host ?port:!Config.port ?user:!Config.user
      ?password:!Config.password ?database:(Some !Config.database)
      ?unix_domain_socket_dir:!Config.unix_domain_socket_dir
      ()
  in
  PGOCaml.set_private_data dbhandle @@ Hashtbl.create 8;
  dbhandle

let ( >> ) f g =
  let _ = f in
  g

let conn_pool : (string, unit) Hashtbl.t PGOCaml.t Lwt_pool.t ref =
  let dispose db = try PGOCaml.close db with _ -> () in
  (* This connection pool will be overwritten by init_fun! *)
  ref
    (Lwt_pool.create !Config.size_conn_pool ~validate:PGOCaml.alive ~dispose
       (fun () -> raise (Failure "Ocsipersist db not initialised")))

let use_pool f =
  Lwt_pool.use !conn_pool @@ fun db ->
  try f db with
  | PGOCaml.Error msg as e ->
      Logs.err ~src:section (fun fmt -> fmt "postgresql protocol error: %s" msg);
      PGOCaml.close db;
      raise e
  | Lwt.Canceled as e ->
      Logs.err ~src:section (fun fmt -> fmt "thread canceled");
      PGOCaml.close db;
      raise e
  | e -> raise e

(* escapes characters that are not in the range of 0x20..0x7e;
   this is to meet PostgreSQL's format requirements for text fields
   while keeping the key column readable whenever possible. *)
let escape_string s =
  let len = String.length s in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    let c = s.[i] in
    let cc = Char.code c in
    if cc < 0x20 || cc > 0x7e then Buffer.add_string buf (sprintf "\\%03o" cc)
      (* non-print -> \ooo *)
    else if c = '\\' then Buffer.add_string buf "\\\\" (* \ -> \\ *)
    else Buffer.add_char buf c
  done;
  Buffer.contents buf

let unescape_string str =
  let is_first_oct_digit c = c >= '0' && c <= '3'
  and is_oct_digit c = c >= '0' && c <= '7'
  and oct_val c = Char.code c - 0x30 in
  let len = String.length str in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    let c = str.[!i] in
    if c = '\\' then (
      incr i;
      if !i < len && str.[!i] = '\\' then (
        Buffer.add_char buf '\\';
        incr i)
      else if
        !i + 2 < len
        && is_first_oct_digit str.[!i]
        && is_oct_digit str.[!i + 1]
        && is_oct_digit str.[!i + 2]
      then (
        let byte = oct_val str.[!i] in
        incr i;
        let byte = (byte lsl 3) + oct_val str.[!i] in
        incr i;
        let byte = (byte lsl 3) + oct_val str.[!i] in
        incr i;
        Buffer.add_char buf (Char.chr byte)))
    else (
      incr i;
      Buffer.add_char buf c)
  done;
  Buffer.contents buf

type 'a parameter = Key of string | Value of 'a

let pack = function
  | Key k -> escape_string k
  | Value v -> PGOCaml.string_of_bytea @@ Marshal.to_string v []

let unpack_value value = Marshal.from_string (PGOCaml.bytea_of_string value) 0

let rec list_last l =
  match l with [ x ] -> x | _ :: r -> list_last r | [] -> raise Not_found

(* get one value from the result of a query *)
let one_value = function
  | [ Some value ] :: _xs -> unpack_value value
  | _ -> raise Not_found

let prepare db query =
  let hashtbl = PGOCaml.private_data db in
  (* Get a unique name for this query using an MD5 digest. *)
  let name = Digest.to_hex (Digest.string query) in
  (* Have we prepared this statement already?  If not, do so. *)
  let is_prepared = Hashtbl.mem hashtbl name in
  if is_prepared then ()
  else PGOCaml.prepare db ~name ~query () >> Hashtbl.add hashtbl name ();
  name

let exec db query params =
  let name = prepare db query in
  let params = List.map (fun x -> Some (pack x)) params in
  PGOCaml.execute db ~name ~params ()

let exec_ db query params = exec db query params >> ()

module Functorial = struct
  type internal = string

  module type COLUMN = sig
    type t

    val column_type : string
    val encode : t -> internal
    val decode : internal -> t
  end

  module Table
      (T : sig
        val name : string
      end)
      (Key : COLUMN)
      (Value : COLUMN) : TABLE with type key = Key.t and type value = Value.t =
  struct
    type key = Key.t
    type value = Value.t

    let name = T.name

    module Aux = struct
      let exec_opt db query params =
        let name = prepare db query in
        PGOCaml.execute db ~name ~params ()

      let exec db query params =
        let name = prepare db query in
        let params = List.map (fun x -> Some x) params in
        PGOCaml.execute db ~name ~params ()

      let exec_ db query params = exec db query params >> ()
      let encode_pair key value = [ Key.encode key; Value.encode value ]
    end

    let init =
      let create_table table db =
        let query =
          sprintf
            "CREATE TABLE IF NOT EXISTS %s (key %s, value %s, PRIMARY KEY \
             (key))"
            table Key.column_type Value.column_type
        in
        Aux.exec_ db query []
      in
      lazy (use_pool @@ create_table T.name)

    let with_table f =
      Lazy.force init;
      use_pool f

    let find key =
      with_table @@ fun db ->
      let query = sprintf "SELECT value FROM %s WHERE key = $1 " name in
      match Aux.exec db query [ Key.encode key ] with
      | [ Some value ] :: _ -> Value.decode value
      | _ -> raise Not_found

    let add key value =
      with_table @@ fun db ->
      let query =
        sprintf
          "INSERT INTO %s VALUES ($1, $2)\n\
          \                           ON CONFLICT (key) DO UPDATE SET value = \
           $2"
          name
      in
      Aux.exec_ db query @@ Aux.encode_pair key value

    let replace_if_exists key value =
      with_table @@ fun db ->
      let query =
        sprintf "UPDATE %s SET value = $2 WHERE key = $1 RETURNING 0" name
      in
      match Aux.exec db query (Aux.encode_pair key value) with
      | [] -> raise Not_found
      | _ -> ()

    let remove key =
      with_table @@ fun db ->
      let query = sprintf "DELETE FROM %s WHERE key = $1" name in
      Aux.exec_ db query [ Key.encode key ]

    let modify_opt key f =
      with_table @@ fun db ->
      let query = sprintf "SELECT value FROM %s WHERE key = $1" name in
      let value = Aux.exec db query [ Key.encode key ] in
      let old_value =
        match value with [ Some v ] :: _ -> Some (Value.decode v) | _ -> None
      in
      let new_value = f old_value in
      match (new_value = old_value, new_value) with
      | true, _ -> ()
      | false, Some new_value ->
          let query =
            sprintf
              "INSERT INTO %s VALUES ($1, $2)\n\
              \                               ON CONFLICT (key) DO UPDATE SET \
               value = $2"
              name
          in
          Aux.exec_ db query @@ Aux.encode_pair key new_value
      | false, None ->
          let query = sprintf "DELETE FROM %s WHERE key = $1" name in
          Aux.exec_ db query [ Key.encode key ]

    let length () =
      with_table @@ fun db ->
      let query = sprintf "SELECT count (1) FROM %s" name in
      one_value (Aux.exec db query [])

    let max_iter_block_size = 1000L

    let rec iter_rec last ?count ?gt ?geq ?lt ?leq f =
      match count with
      | Some c when c <= 0L -> ()
      | _ ->
          let key_value_of_row = function
            | [ Some key; Some value ] -> (Key.decode key, Value.decode value)
            | _ -> raise Ocsipersist_error
          in
          let query =
            sprintf
              "SELECT * FROM %s\n\
              \                           WHERE ($1 :: %s IS NULL OR key > $1)\n\
              \                           AND ($2 :: %s IS NULL OR key > $2)\n\
              \                           AND ($3 :: %s IS NULL OR key >= $3)\n\
              \                           AND ($4 :: %s IS NULL OR key < $4)\n\
              \                           AND ($5 :: %s IS NULL OR key <= $5)\n\
              \                           ORDER BY key LIMIT $6"
              name Key.column_type Key.column_type Key.column_type
              Key.column_type Key.column_type
          in
          let limit =
            match count with
            | Some c when c <= max_iter_block_size -> c
            | _ -> max_iter_block_size
          in
          let args =
            [
              Option.map Key.encode last;
              Option.map Key.encode gt;
              Option.map Key.encode geq;
              Option.map Key.encode lt;
              Option.map Key.encode leq;
              Some (Int64.to_string limit);
            ]
          in
          let l = with_table (fun db -> Aux.exec_opt db query args) in
          let key_values = List.map key_value_of_row l in
          f key_values;
          if Int64.of_int (List.length l) < limit then ()
          else
            let last, (_ : value) = list_last key_values in
            let count =
              Option.map
                Int64.(fun c -> sub c @@ of_int @@ List.length key_values)
                count
            in
            iter_rec (Some last) ?count f ?gt ?geq ?lt ?leq

    let iter_batch = iter_rec None

    let iter ?count ?gt ?geq ?lt ?leq f =
      let f key_values = List.iter (fun (k, v) -> f k v) key_values in
      iter_rec None ?count ?gt ?geq ?lt ?leq f

    let fold ?count ?gt ?geq ?lt ?leq f x =
      let res = ref x in
      let g key value =
        let res' = f key value !res in
        res := res'
      in
      iter ?count ?gt ?geq ?lt ?leq g >> !res

    let iter_block ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ =
      failwith "Ocsipersist.iter_block: not implemented"

    module Variable = Ocsipersist_lib.Variable (struct
      type k = key
      type v = value

      let find = find
      let add = add
    end)
  end

  module Column = struct
    module String : COLUMN with type t = string = struct
      type t = string

      let column_type = "text"
      let encode = escape_string
      let decode = unescape_string
    end

    module Float : COLUMN with type t = float = struct
      type t = float

      let column_type = "float"
      let encode = Printf.sprintf "%.16f"
      let decode = Stdlib.float_of_string
    end

    module Marshal (C : sig
      type t
    end) : COLUMN with type t = C.t = struct
      type t = C.t

      let column_type = "bytea"
      let encode v = PGOCaml.string_of_bytea @@ Marshal.to_string v []
      let decode v = Marshal.from_string (PGOCaml.bytea_of_string v) 0
    end
  end
end

module Polymorphic = Ocsipersist_lib.Polymorphic (Functorial)

type 'value table = 'value Polymorphic.table

module Store = struct
  type store = string
  type 'a t = { store : string; name : string }

  let open_store store =
    use_pool @@ fun db ->
    let create_table db table =
      let query =
        sprintf
          "CREATE TABLE IF NOT EXISTS %s (key TEXT, value BYTEA, PRIMARY \
           KEY(key))"
          table
      in
      exec_ db query []
    in
    create_table db store >> store

  let make_persistent_worker ~store ~name ~default db =
    let query =
      sprintf
        "INSERT INTO %s VALUES ( $1 , $2 )\n\
        \                         ON CONFLICT ( key ) DO NOTHING"
        store
    in
    (* NOTE: incompatible with < 9.5 *)
    exec_ db query [ Key name; Value default ] >> { store; name }

  let make_persistent ~store ~name ~default =
    use_pool @@ fun db -> make_persistent_worker ~store ~name ~default db

  let make_persistent_lazy_lwt ~store ~name ~default =
    use_pool @@ fun db ->
    let query = sprintf "SELECT 1 FROM %s WHERE key = $1 " store in
    match exec db query [ Key name ] with
    | [] ->
        let default = default () in
        make_persistent_worker ~store ~name ~default db
    | _ -> { store; name }

  let make_persistent_lazy ~store ~name ~default =
    let default () = default () in
    make_persistent_lazy_lwt ~store ~name ~default

  let get p =
    use_pool @@ fun db ->
    let query = sprintf "SELECT value FROM %s WHERE key = $1 " p.store in
    one_value (exec db query [ Key p.name ])

  let set p v =
    use_pool @@ fun db ->
    let query = sprintf "UPDATE %s SET value = $2 WHERE key = $1 " p.store in
    exec db query [ Key p.name; Value v ] >> ()
end

module Ref = Ocsipersist_lib.Ref (Store)

type store = Store.store
type 'a variable = 'a Store.t

let init () =
  conn_pool :=
    Lwt_pool.create !Config.size_conn_pool ~validate:PGOCaml.alive connect
