open Eio.Std

(** This modules provides tools for creating more implementations of the {!Ocsipersist} virtual module. *)

let current_switch : Switch.t Fiber.key = Fiber.create_key ()

module Sigs = struct
  module type TABLE = sig
    type key
    type value

    val name : string
    val find : key -> value
    val add : key -> value -> unit
    val replace_if_exists : key -> value -> unit
    val remove : key -> unit
    val modify_opt : key -> (value option -> value option) -> unit
    val length : unit -> int

    val iter :
      ?count:int64 ->
      ?gt:key ->
      ?geq:key ->
      ?lt:key ->
      ?leq:key ->
      (key -> value -> unit) ->
      unit

    val fold :
      ?count:int64 ->
      ?gt:key ->
      ?geq:key ->
      ?lt:key ->
      ?leq:key ->
      (key -> value -> 'a -> 'a) ->
      'a ->
      'a

    val iter_block :
      ?count:int64 ->
      ?gt:key ->
      ?geq:key ->
      ?lt:key ->
      ?leq:key ->
      (key -> value -> unit) ->
      unit

    val iter_batch :
      ?count:int64 ->
      ?gt:key ->
      ?geq:key ->
      ?lt:key ->
      ?leq:key ->
      ((key * value) list -> unit) ->
      unit

    module Variable : sig
      type t

      val make : name:key -> default:value -> t
      val make_lazy : name:key -> default:(unit -> value) -> t
      val make_lazy_lwt : name:key -> default:(unit -> value) -> t
      val get : t -> value
      val set : t -> value -> unit
    end
  end

  module type FUNCTORIAL = sig
    type internal

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
        (Value : COLUMN) : TABLE with type key = Key.t and type value = Value.t

    module Column : sig
      module String : COLUMN with type t = string
      module Float : COLUMN with type t = float

      module Marshal (C : sig
        type t
      end) : COLUMN with type t = C.t
    end
  end

  module type POLYMORPHIC = sig
    type 'value table
    (** Type of persistent table *)

    val table_name : 'value table -> string
    (** returns the name of the table  *)

    val open_table : string -> 'value table
    (** Open a table (and create it if it does not exist)  *)

    val find : 'value table -> string -> 'value
    (** [find table key] gives the value associated to [key].
        Fails with [Not_found] if not found. *)

    val add : 'value table -> string -> 'value -> unit
    (** [add table key value] associates [value] to [key].
        If the database already contains data associated with [key],
        that data is discarded and silently replaced by the new data.
    *)

    val replace_if_exists : 'value table -> string -> 'value -> unit
    (** [replace_if_exists table key value]
        associates [value] to [key] only if [key] is already bound.
        If the database does not contain any data associated with [key],
        fails with [Not_found].
    *)

    val remove : 'value table -> string -> unit
    (** [remove table key] removes the entry in the table if it exists *)

    val length : 'value table -> int
    (** Size of a table. *)

    val iter_step : (string -> 'a -> unit) -> 'a table -> unit
    (** Important warning: this iterator may not iter on all data of the table
        if another thread is modifying it in the same time. Nonetheless, it should
        not miss more than a very few data from time to time, except if the table
        is very old (at least 9 223 372 036 854 775 807 insertions).
    *)

    val fold_step : (string -> 'a -> 'b -> 'b) -> 'a table -> 'b -> 'b
    (** Important warning: this iterator may not iter on all data of the table
        if another thread is modifying it in the same time. Nonetheless, it should
        not miss more than a very few data from time to time, except if the table
        is very old (at least 9 223 372 036 854 775 807 insertions).
    *)

    val iter_block : (string -> 'a -> unit) -> 'a table -> unit
    (** MAJOR WARNING: Unlike iter_step, this iterator won't miss any
        entry and will run in one shot. It is therefore more efficient, BUT:
        it will lock the WHOLE database during its execution,
        thus preventing ANYBODY from accessing it (including the function f
        which is iterated).
        As a consequence: you MUST NOT use any function from ocsipersist in f,
        otherwise you would lock yourself and everybody else! Be VERY cautious.
    *)
  end

  module type REF = sig
    (** Persistent references for OCaml *)

    type 'a t
    (** The type of (persistent or not) references *)

    val ref : ?persistent:string -> 'a -> 'a t
    (** [ref ?persistent default] creates a reference.
        If optional parameter [?persistent] is absent,
        the reference will not be persistent (implemented using OCaml references).
        Otherwise, the value of [persistent] will be used as key for the
        value in the persistent reference table.
        If the reference already exists, the current value is kept.
        Be careful to change this name every time you change the type of the
        value. *)

    val get : 'a t -> 'a
    (** Get the value of a reference *)

    val set : 'a t -> 'a -> unit
    (** Set the value of a reference *)
  end

  module type STORE = sig
    type 'a t
    (** Type of persistent data *)

    type store
    (** Data are divided into stores.
        Create one store for your project, where you will save all your data. *)

    val open_store : string -> store
    (** Open a store (and create it if it does not exist)  *)

    val make_persistent : store:store -> name:string -> default:'a -> 'a t
    (** [make_persistent store name default] find a persistent value
        named [name] in store [store]
        from database, or create it with the default value [default] if it
        does not exist. *)

    val make_persistent_lazy :
      store:store -> name:string -> default:(unit -> 'a) -> 'a t
    (** Same as make_persistent but the default value is evaluated only
        if needed
    *)

    val make_persistent_lazy_lwt :
      store:store -> name:string -> default:(unit -> 'a) -> 'a t
    (** Lwt version of make_persistent_lazy.
    *)

    val get : 'a t -> 'a
    (** [get pv] gives the value of [pv] *)

    val set : 'a t -> 'a -> unit
    (** [set pv value] sets a persistent value [pv] to [value] *)
  end
end

open Sigs

(** deriving polymorphic interface from the functorial one *)
module Polymorphic (Functorial : FUNCTORIAL) : POLYMORPHIC = struct
  module type POLYMORPHIC = TABLE with type key = string

  type 'value table = (module POLYMORPHIC with type value = 'value)

  let open_table (type a) name =
    let open Functorial in
    let module T =
      Table
        (struct
          let name = name
        end)
        (Column.String)
        (Column.Marshal (struct
          type t = a
        end))
    in
    (module T : POLYMORPHIC with type value = a)

  let table_name (type a) (module T : POLYMORPHIC with type value = a) = T.name
  let find (type a) (module T : POLYMORPHIC with type value = a) = T.find
  let add (type a) (module T : POLYMORPHIC with type value = a) = T.add

  let replace_if_exists (type a) (module T : POLYMORPHIC with type value = a) =
    T.replace_if_exists

  let remove (type a) (module T : POLYMORPHIC with type value = a) = T.remove
  let length (type a) (module T : POLYMORPHIC with type value = a) = T.length ()

  let iter_step (type a) f (module T : POLYMORPHIC with type value = a) =
    T.iter f

  let fold_step (type a) f (module T : POLYMORPHIC with type value = a) =
    T.fold f

  let iter_block (type a) f (module T : POLYMORPHIC with type value = a) =
    T.iter_block f
end

module Variable (T : sig
  type k
  type v

  val find : k -> v
  val add : k -> v -> unit
end) =
struct
  type t = { name : T.k; default : unit -> T.v }

  let make_lazy_lwt ~name ~default = { name; default }
  let make_lazy ~name ~default = { name; default = (fun () -> default ()) }
  let make ~name ~default = { name; default = (fun () -> default) }

  let get { name; default } =
    try T.find name
    with Not_found ->
      let d = default () in
      T.add name d;
      d

  let set { name } = T.add name
end

module Ref (Store : STORE) = struct
  let store = lazy (Store.open_store "__ocsipersist_ref_store__")

  type 'a t = Ref of 'a ref | Per of 'a Store.t Lazy.t

  let ref ?persistent v =
    match persistent with
    | None -> Ref (ref v)
    | Some name ->
        Per
          (lazy
            (let store = Lazy.force store in
             Store.make_persistent ~store ~name ~default:v))

  let get = function
    | Ref r -> !r
    | Per r ->
        let (lazy r) = r in
        Store.get r

  let set r v =
    match r with
    | Ref r -> r := v
    | Per r ->
        let (lazy r) = r in
        Store.set r v
end
