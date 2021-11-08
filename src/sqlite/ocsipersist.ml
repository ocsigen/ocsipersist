module type TABLE = Ocsipersist_lib.Sigs.TABLE

let section = Lwt_log.Section.make "ocsigen:ocsipersist:sqlite"

open Lwt.Infix
open Sqlite3
open Printf

module Aux = struct
  (* This reference is overwritten when the init function (at the end of the file)
   is run, which occurs when the extension is loaded *)
  let db_file = ref (Ocsigen_config.get_datadir () ^ "/ocsidb")
  let yield () = Thread.yield ()

  let rec bind_safely stmt = function
    | [] -> stmt
    | (value, name) :: q as l -> (
      match Sqlite3.bind stmt (bind_parameter_index stmt name) value with
      | Rc.OK -> bind_safely stmt q
      | Rc.BUSY | Rc.LOCKED -> yield (); bind_safely stmt l
      | rc ->
          ignore (finalize stmt);
          failwith (Rc.to_string rc))

  let close_safely db =
    if not (db_close db)
    then Lwt_log.ign_error ~section "Couldn't close database"

  let m = Mutex.create ()

  let exec_safely f =
    let aux () =
      let db =
        Mutex.lock m;
        try db_open !db_file with e -> Mutex.unlock m; raise e
      in
      try
        let r = f db in
        close_safely db; Mutex.unlock m; r
      with e -> close_safely db; Mutex.unlock m; raise e
    in
    Lwt_preemptive.detach aux ()

  (* Référence indispensable pour les codes de retours et leur signification :
   * http://sqlite.org/capi3ref.html
   * Langage compris par SQLite : http://www.sqlite.org/lang.html
   *)

  let db_create table =
    let sql =
      sprintf
        "CREATE TABLE IF NOT EXISTS %s (key TEXT, value BLOB,  PRIMARY KEY(key) ON CONFLICT REPLACE)"
        table
    in
    let create db =
      let stmt = prepare db sql in
      let rec aux () =
        match step stmt with
        | Rc.DONE -> ignore (finalize stmt)
        | Rc.BUSY | Rc.LOCKED -> yield (); aux ()
        | rc ->
            ignore (finalize stmt);
            failwith (Rc.to_string rc)
      in
      aux ()
    in
    exec_safely create >>= fun () -> Lwt.return table

  let db_get, db_replace =
    let get (table, key) db =
      let sqlget = sprintf "SELECT value FROM %s WHERE key = :key " table in
      let stmt = bind_safely (prepare db sqlget) [Data.TEXT key, ":key"] in
      let rec aux () =
        match step stmt with
        | Rc.ROW ->
            let value =
              match column stmt 0 with Data.BLOB s -> s | _ -> assert false
            in
            ignore (finalize stmt);
            value
        | Rc.DONE ->
            ignore (finalize stmt);
            raise Not_found
        | Rc.BUSY | Rc.LOCKED -> yield (); aux ()
        | rc ->
            ignore (finalize stmt);
            failwith (Rc.to_string rc)
      in
      aux ()
    in
    let replace (table, key) value db =
      let sqlreplace =
        sprintf "INSERT INTO %s VALUES ( :key , :value )" table
      in
      let stmt =
        bind_safely (prepare db sqlreplace)
          [Data.TEXT key, ":key"; Data.BLOB value, ":value"]
      in
      let rec aux () =
        match step stmt with
        | Rc.DONE -> ignore (finalize stmt)
        | Rc.BUSY | Rc.LOCKED -> yield (); aux ()
        | rc ->
            ignore (finalize stmt);
            failwith (Rc.to_string rc)
      in
      aux ()
    in
    ( (fun tablekey -> exec_safely (get tablekey))
    , fun tablekey value -> exec_safely (replace tablekey value) )
end

module Store = struct
  type store = string
  type 'a t = store * string

  let open_store name =
    let s = "store___" ^ name in
    Aux.db_create s

  let make_persistent_lazy_lwt ~store ~name ~default =
    let pvname = store, name in
    Lwt.catch
      (fun () -> Aux.db_get pvname >>= fun _ -> Lwt.return ())
      (function
        | Not_found ->
            default () >>= fun def ->
            Aux.db_replace pvname (Marshal.to_string def [])
        | e -> Lwt.fail e)
    >>= fun () -> Lwt.return pvname

  let make_persistent_lazy ~store ~name ~default =
    let default () = Lwt.wrap default in
    make_persistent_lazy_lwt ~store ~name ~default

  let make_persistent ~store ~name ~default =
    make_persistent_lazy ~store ~name ~default:(fun () -> default)

  let get (pvname : 'a t) : 'a =
    Aux.db_get pvname >>= fun r -> Lwt.return (Marshal.from_string r 0)

  let set pvname v =
    let data = Marshal.to_string v [] in
    Aux.db_replace pvname data
end

type store = Store.store
type 'a variable = 'a Store.t

module Functorial = struct
  type internal = Data.t

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
  (Value : COLUMN) :
    Ocsipersist_lib.Sigs.TABLE with type key = Key.t and type value = Value.t =
  struct
    type key = Key.t
    type value = Value.t

    let name = "store___" ^ T.name

    let init =
      let create db =
        let sql =
          sprintf
            "CREATE TABLE IF NOT EXISTS %s
             (key %s, value %s, PRIMARY KEY (key) ON CONFLICT REPLACE)"
            name Key.column_type Value.column_type
        in
        let stmt = prepare db sql in
        let rec aux () =
          match step stmt with
          | Rc.DONE -> ignore (finalize stmt)
          | Rc.BUSY | Rc.LOCKED -> Aux.yield (); aux ()
          | rc ->
              ignore (finalize stmt);
              failwith (Rc.to_string rc)
        in
        aux ()
      in
      lazy (Aux.exec_safely create)

    let with_table f = Lazy.force init >>= fun () -> Aux.exec_safely f

    let db_get key db =
      let sqlget = sprintf "SELECT value FROM %s WHERE key = :key" name in
      let stmt = Aux.bind_safely (prepare db sqlget) [Key.encode key, ":key"] in
      let rec aux () =
        match step stmt with
        | Rc.ROW ->
            let value = column stmt 0 in
            ignore (finalize stmt);
            value
        | Rc.DONE ->
            ignore (finalize stmt);
            raise Not_found
        | Rc.BUSY | Rc.LOCKED -> Aux.yield (); aux ()
        | rc ->
            ignore (finalize stmt);
            failwith (Rc.to_string rc)
      in
      Value.decode @@ aux ()

    let db_replace key value db =
      let sqlreplace = sprintf "INSERT INTO %s VALUES (:key, :value)" name in
      let stmt =
        Aux.bind_safely (prepare db sqlreplace)
          [Key.encode key, ":key"; Value.encode value, ":value"]
      in
      let rec aux () =
        match step stmt with
        | Rc.DONE -> ignore (finalize stmt)
        | Rc.BUSY | Rc.LOCKED -> Aux.yield (); aux ()
        | rc ->
            ignore (finalize stmt);
            failwith (Rc.to_string rc)
      in
      aux ()

    let db_remove key db =
      let sql = sprintf "DELETE FROM %s WHERE key = :key " name in
      let stmt = Aux.bind_safely (prepare db sql) [Key.encode key, ":key"] in
      let rec aux () =
        match step stmt with
        | Rc.DONE -> ignore (finalize stmt)
        | Rc.BUSY | Rc.LOCKED -> Aux.yield (); aux ()
        | rc ->
            ignore (finalize stmt);
            failwith (Rc.to_string rc)
      in
      aux ()

    let db_length table db =
      let sql = sprintf "SELECT count (1) FROM %s " table in
      let stmt = prepare db sql in
      let rec aux () =
        match step stmt with
        | Rc.ROW ->
            let value =
              match column stmt 0 with
              | Data.INT s -> Int64.to_int s
              | _ -> assert false
            in
            ignore (finalize stmt);
            value
        | Rc.DONE ->
            ignore (finalize stmt);
            raise Not_found
        | Rc.BUSY | Rc.LOCKED -> Aux.yield (); aux ()
        | rc ->
            ignore (finalize stmt);
            failwith (Rc.to_string rc)
      in
      aux ()

    let db_iter ?gt ?geq ?lt ?leq table rowid db =
      let sql =
        sprintf
          "SELECT key, value, ROWID FROM %s
                 WHERE ROWID > :rowid
                 AND coalesce (key > :gt, true)
                 AND coalesce (key >= :geq, true)
                 AND coalesce (key < :lt, true)
                 AND coalesce (key <= :leq, true)"
          table
      in
      let encode_key_opt = function
        | Some k -> Key.encode k
        | None -> Data.NULL
      in
      let gt_sql = encode_key_opt gt
      and geq_sql = encode_key_opt geq
      and lt_sql = encode_key_opt lt
      and leq_sql = encode_key_opt leq in
      let stmt =
        Aux.bind_safely (prepare db sql)
          [ Data.INT rowid, ":rowid"
          ; gt_sql, ":gt"
          ; geq_sql, ":geq"
          ; lt_sql, ":lt"
          ; leq_sql, ":leq" ]
      in
      let rec aux () =
        match step stmt with
        | Rc.ROW -> (
          match column stmt 0, column stmt 1, column stmt 2 with
          | k, v, Data.INT rowid ->
              ignore (finalize stmt);
              Some (k, v, rowid)
          | _ -> assert false)
        | Rc.DONE ->
            ignore (finalize stmt);
            None
        | Rc.BUSY | Rc.LOCKED -> Aux.yield (); aux ()
        | rc ->
            ignore (finalize stmt);
            failwith (Rc.to_string rc)
      in
      aux ()

    let find k = with_table @@ db_get k
    let add k v = with_table @@ db_replace k v
    let add_batch _ = failwith __LOC__

    let replace_if_exists k v =
      with_table @@ fun db ->
      ignore (db_get k db);
      db_replace k v db

    let remove key = with_table @@ db_remove key

    let modify_opt key f =
      with_table @@ fun db ->
      let old_value = try Some (db_get key db) with Not_found -> None in
      match f old_value with
      | Some new_value -> db_replace key new_value db
      | None -> db_remove key db

    let fold ?count ?gt ?geq ?lt ?leq f beg =
      let i = ref 0L in
      let rec aux rowid beg =
        match count with
        | Some c when !i >= c -> Lwt.return beg
        | _ -> (
            i := Int64.succ !i;
            with_table (db_iter ?gt ?geq ?lt ?leq name rowid) >>= function
            | None -> Lwt.return beg
            | Some (k, v, rowid') ->
                f (Key.decode k) (Value.decode v) beg >>= aux rowid')
      in
      aux Int64.zero beg

    let iter ?count ?gt ?geq ?lt ?leq f =
      fold ?count ?gt ?geq ?lt ?leq (fun k v () -> f k v) ()

    let iter_batch ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ =
      failwith "Ocsipersist.iter_batch not implemented for SQLite"

    let iter_block ?count ?gt ?geq ?lt ?leq f =
      let sql =
        sprintf
          "SELECT key, value FROM %s
         WHERE coalesce (key > :gt, true)
           AND coalesce (key >= :geq, true)
           AND coalesce (key < :lt, true)
           AND coalesce (key <= :leq, true)
         LIMIT coalesce (:count, -1)"
          name
      in
      let encode_key_opt = function
        | Some k -> Key.encode k
        | None -> Data.NULL
      in
      let gt_sql = encode_key_opt gt
      and geq_sql = encode_key_opt geq
      and lt_sql = encode_key_opt lt
      and leq_sql = encode_key_opt leq in
      let count_sql =
        match count with Some c -> Data.INT c | None -> Data.NULL
      in
      let iter db =
        let stmt =
          Aux.bind_safely (prepare db sql)
            [ gt_sql, ":gt"
            ; geq_sql, ":geq"
            ; lt_sql, ":lt"
            ; leq_sql, ":leq"
            ; count_sql, ":count" ]
        in
        let rec aux () =
          match step stmt with
          | Rc.ROW ->
              f (Key.decode @@ column stmt 0) (Value.decode @@ column stmt 1);
              aux ()
          | Rc.DONE -> ignore (finalize stmt)
          | Rc.BUSY | Rc.LOCKED -> Aux.yield (); aux ()
          | rc ->
              ignore (finalize stmt);
              failwith (Rc.to_string rc)
        in
        aux ()
      in
      with_table iter

    let length () = with_table @@ db_length name

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
      let encode s = Data.TEXT s
      let decode = function Data.TEXT f -> f | _ -> assert false
    end

    module Float : COLUMN with type t = float = struct
      type t = float

      let column_type = "float"
      let encode f = Data.FLOAT f
      let decode = function Data.FLOAT f -> f | _ -> assert false
    end

    module Marshal (C : sig
      type t
    end) : COLUMN with type t = C.t = struct
      type t = C.t

      let column_type = "blob"
      let encode v = Data.BLOB (Marshal.to_string v [])

      let decode = function
        | Data.BLOB v -> Marshal.from_string v 0
        | _ -> assert false
    end
  end
end

module Polymorphic = Ocsipersist_lib.Polymorphic (Functorial)

type 'value table = 'value Polymorphic.table

module Registration = struct
  let parse_global_config = function
    | [] -> None
    | [Xml.Element ("database", [("file", s)], [])] -> Some s
    | _ ->
        raise
          (Ocsigen_extensions.Error_in_config_file
             "Unexpected content inside Ocsipersist config")

  let init config =
    Aux.db_file := Ocsigen_config.get_datadir () ^ "/ocsidb";
    (match parse_global_config config with
    | None -> ()
    | Some d -> Aux.db_file := d);
    (* We check that we can access the database *)
    try Lwt_main.run (Aux.exec_safely (fun _ -> ()))
    with e ->
      Ocsigen_messages.errlog
        (Printf.sprintf
           "Error opening database file '%s' when registering Ocsipersist. Check that the directory exists, and that Ocsigen has enough rights"
           !Aux.db_file);
      raise e

  let _ = Ocsigen_extensions.register ~name:"ocsipersist" ~init_fun:init ()
end
