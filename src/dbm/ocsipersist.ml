open Eio.Std

(* FIX: the log file is never reopened *)

open Ocsidbmtypes

module type TABLE = Ocsipersist_lib.Sigs.TABLE

let section = Logs.Src.create "ocsigen:ocsipersist:dbm"

exception Ocsipersist_error

let socketname = "socket"

module Config = Ocsipersist_settings

module Aux = struct
  external sys_exit : int -> 'a = "caml_sys_exit"
end

module Db = struct
  let try_connect sname =
    try
      let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      Unix.connect
        (* TODO: lwt-to-direct-style: This call to [Unix.connect] was [Lwt_unix.connect] before. It's now blocking. *)
        socket (Unix.ADDR_UNIX sname);
      socket
    with _ ->
      Logs.warn ~src:section (fun fmt ->
          fmt "Launching a new Ocsidbm process: %s on directory %s."
            !Config.ocsidbm !Config.directory);
      let param = [| !Config.ocsidbm; !Config.directory |] in
      let child () =
        let log =
          Unix.openfile !Config.error_log_path
            [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
            0o640
        in
        Unix.dup2 log Unix.stderr;
        Unix.close log;
        let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
        Unix.dup2 devnull Unix.stdout;
        Unix.close devnull;
        Unix.close Unix.stdin;
        Unix.execvp !Config.ocsidbm param
      in
      let pid = Lwt_unix.fork () in
      if pid = 0 then
        if
          (* double fork *)
          Lwt_unix.fork () = 0
        then child ()
        else Aux.sys_exit 0
      else
        let _ = Unix.waitpid [] pid in
        Eio_unix.sleep 1.1;
        let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        Unix.connect
          (* TODO: lwt-to-direct-style: This call to [Unix.connect] was [Lwt_unix.connect] before. It's now blocking. *)
          socket (Unix.ADDR_UNIX sname);
        socket

  let rec get_indescr i =
    try try_connect (!Config.directory ^ "/" ^ socketname)
    with e ->
      if i = 0 then (
        Logs.err ~src:section (fun fmt ->
            fmt
              "Cannot connect to Ocsidbm. Will continue without persistent \
               session support. Error message is: %s .Have a look at the logs \
               to see if there is an error message from the Ocsidbm process."
              (match e with
              | Unix.Unix_error (a, b, c) ->
                  Printf.sprintf "%a in %s(%s)"
                    (fun () -> Unix.error_message)
                    a b c
              | _ -> Printexc.to_string e));
        raise e)
      else (
        Eio_unix.sleep 2.1;
        get_indescr (i - 1))

  let send =
    let previous = ref Ok in
    fun v ->
      let _ = try !previous with _ -> Ok in
      let inch = !Config.inch in
      let outch = !Config.outch in
      previous :=
        (Marshal.to_channel outch v;
         Eio.Buf_write.flush outch;
         Marshal.from_channel inch);
      !previous

  let get (store, name) =
    match send (Get (store, name)) with
    | Value v -> v
    | Dbm_not_found -> raise Not_found
    | Error e -> raise e
    | _ -> raise Ocsipersist_error

  let remove (store, name) =
    match send (Remove (store, name)) with
    | Ok -> ()
    | Error e -> raise e
    | _ -> raise Ocsipersist_error

  let replace (store, name) value =
    match send (Replace (store, name, value)) with
    | Ok -> ()
    | Error e -> raise e
    | _ -> raise Ocsipersist_error

  let replace_if_exists (store, name) value =
    match send (Replace_if_exists (store, name, value)) with
    | Ok -> ()
    | Dbm_not_found -> raise Not_found
    | Error e -> raise e
    | _ -> raise Ocsipersist_error

  let firstkey store =
    match send (Firstkey store) with
    | Key k -> Some k
    | Error e -> raise e
    | _ -> None

  let nextkey store =
    match send (Nextkey store) with
    | Key k -> Some k
    | Error e -> raise e
    | _ -> None

  let length store =
    match send (Length store) with
    | Value v -> Marshal.from_string v 0
    | Dbm_not_found -> 0
    | Error e -> raise e
    | _ -> raise Ocsipersist_error
end

module Store = struct
  type store = string

  type 'a t = store * string
  (** Type of persistent data *)

  let open_store name = name

  let make_persistent_lazy_lwt ~store ~name ~default =
    let pvname = (store, name) in
    (try
       let _ = Db.get pvname in
       ()
     with
    | Not_found ->
        let def = default () in
        Db.replace pvname (Marshal.to_string def [])
    | e -> raise e);
    pvname

  let make_persistent_lazy ~store ~name ~default =
    let default () = default () in
    make_persistent_lazy_lwt ~store ~name ~default

  let make_persistent ~store ~name ~default =
    make_persistent_lazy ~store ~name ~default:(fun () -> default)

  let get (pvname : 'a t) : 'a =
    let r = Db.get pvname in
    Marshal.from_string r 0

  let set pvname v =
    let data = Marshal.to_string v [] in
    Db.replace pvname data
end

type store = Store.store
type 'a variable = 'a Store.t

module Functorial = struct
  type internal = string

  module type COLUMN = sig
    type t

    val column_type : string
    val encode : t -> string
    val decode : string -> t
  end

  module Table
      (T : sig
        val name : string
      end)
      (Key : COLUMN)
      (Value : COLUMN) :
    Ocsipersist_lib.Sigs.TABLE with type key = Key.t and type value = Value.t =
  struct
    type key = Key.t
    type value = Value.t

    let name = T.name
    let find key = Value.decode (Db.get (name, Key.encode key))
    let add key value = Db.replace (name, Key.encode key) (Value.encode value)

    let replace_if_exists key value =
      Db.replace_if_exists (name, Key.encode key) (Value.encode value)

    let remove key = Db.remove (name, Key.encode key)

    let fold ?count ?gt ?geq ?lt ?leq f beg =
      let i = ref 0L in
      let rec aux nextkey beg =
        match count with
        | Some c when !i >= c -> beg
        | _ -> (
            match nextkey name with
            | None -> beg
            | Some k -> (
                let k = Key.decode k in
                match (gt, geq, lt, leq) with
                | _, _, Some lt, _ when k >= lt -> beg
                | _, _, _, Some le when k > le -> beg
                | Some gt, _, _, _ when k <= gt -> aux Db.nextkey beg
                | _, Some ge, _, _ when k < ge -> aux Db.nextkey beg
                | _ ->
                    i := Int64.succ !i;
                    let r = find k in
                    (aux Db.nextkey) (f k r beg)))
      in
      aux Db.firstkey beg

    let iter ?count ?gt ?geq ?lt ?leq f =
      fold ?count ?gt ?geq ?lt ?leq (fun k v () -> f k v) ()

    let iter_batch ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ =
      failwith "Ocsipersist.iter_batch not implemented for DBM"

    let iter_block ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ =
      failwith
        "iter_block not implemented for DBM. Please use Ocsipersist with sqlite"

    let modify_opt key f =
      let old_value =
        try
          let v = find key in
          Some v
        with
        | Not_found -> None
        | _ -> assert false
      in
      match f old_value with
      | None -> remove key
      | Some new_value -> replace_if_exists key new_value

    let length () =
      (* for DBM the result may be less than the actual lengeth *)
      Db.length name

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

      let column_type = "_"
      let encode s = s
      let decode s = s
    end

    module Float : COLUMN with type t = float = struct
      type t = float

      let column_type = "_"
      let encode = string_of_float
      let decode = float_of_string
    end

    module Marshal (C : sig
      type t
    end) : COLUMN with type t = C.t = struct
      type t = C.t

      let column_type = "_"
      let encode v = Marshal.to_string v []
      let decode v = Marshal.from_string v 0
    end
  end
end

module Polymorphic = Ocsipersist_lib.Polymorphic (Functorial)
module Ref = Ocsipersist_lib.Ref (Store)

type 'value table = 'value Polymorphic.table

(* iterator: with a separate connexion:
   exception Exn1
   let iter_table f table =
   let first = Marshal.to_string (Firstkey table) [] in
   let firstl = String.length first in
   let next = Marshal.to_string (Nextkey table) [] in
   let nextl = String.length next in
   (Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 >>=
   (fun socket ->
     Lwt_unix.connect
       (Lwt_unix.Plain socket)
       (Unix.ADDR_UNIX (Config.directory^"/"^socketname)) >>=
     (fun () -> return (Lwt_unix.Plain socket)) >>=
     (fun indescr ->
       let inch = Lwt_unix.in_channel_of_descr indescr in
       let nextkey next nextl =
         Lwt_unix.write indescr next 0 nextl >>=
         (fun l2 -> if l2 <> nextl
         then Lwt.fail Ocsipersist_error
         else (Lwt_unix.input_line inch >>=
               fun answ -> return (Marshal.from_string answ 0)))
       in
       let rec aux n l =
         nextkey n l >>=
         (function
           | End -> return ()
           | Key k -> find table k >>= f k
           | Error e -> Lwt.fail e
           | _ -> Lwt.fail Ocsipersist_error) >>=
         (fun () -> aux next nextl)
       in
       catch
         (fun () ->
           aux first firstl >>=
           (fun () -> Unix.close socket; return ()))
         (fun e -> Unix.close socket; Lwt.fail e))))
*)

let init () =
  if !Ocsipersist_settings.delay_loading then
    Logs.warn ~src:section (fun fmt ->
        fmt "Asynchronuous initialization (may fail later)")
  else Logs.warn ~src:section (fun fmt -> fmt "Initializing ...");
  let indescr = Db.get_indescr 2 in
  if !Ocsipersist_settings.delay_loading then (
    Ocsipersist_settings.inch :=
      (fun x1 ->
        Eio.Buf_read.of_flow ~max_size:1_000_000
          (Eio_unix.Net.import_socket_stream
             ~sw:(Stdlib.Option.get (Fiber.get Ocsipersist_lib.current_switch))
             ~close_unix:true x1
            : [ `R | `Flow | `Close ] r))
        indescr;
    Ocsipersist_settings.outch :=
      (fun x1 ->
        Eio.Buf_write.with_flow
          (Eio_unix.Net.import_socket_stream
             ~sw:(Stdlib.Option.get (Fiber.get Ocsipersist_lib.current_switch))
             ~close_unix:true x1
            : [ `W | `Flow | `Close ] r)
          (fun outbuf -> `Move_writing_code_here))
        (* TODO: lwt-to-direct-style: Write operations to buffered IO should be moved inside [with_flow]. *)
        indescr)
  else
    let r =
      Eio_main.run (fun env ->
          Fiber.with_binding Ocsipersist_lib.env env (fun () ->
              Switch.run (fun sw ->
                  Fiber.with_binding Ocsipersist_lib.current_switch sw
                    (fun () ->
                      (* TODO: lwt-to-direct-style: [Eio_main.run] argument used to be a [Lwt] promise and is now a [fun]. Make sure no asynchronous or IO calls are done outside of this [fun]. *)
                      indescr))))
    in
    Ocsipersist_settings.inch :=
      Eio.Buf_read.of_flow ~max_size:1_000_000
        (Eio_unix.Net.import_socket_stream
           ~sw:(Stdlib.Option.get (Fiber.get Ocsipersist_lib.current_switch))
           ~close_unix:true r
          : [ `R | `Flow | `Close ] r);
    Ocsipersist_settings.outch :=
      Eio.Buf_write.with_flow
        (Eio_unix.Net.import_socket_stream
           ~sw:(Stdlib.Option.get (Fiber.get Ocsipersist_lib.current_switch))
           ~close_unix:true
           (* TODO: lwt-to-direct-style: Write operations to buffered IO should be moved inside [with_flow]. *)
           r
          : [ `W | `Flow | `Close ] r)
        (fun outbuf -> `Move_writing_code_here);
    Logs.warn ~src:section (fun fmt -> fmt "...Initialization complete")
