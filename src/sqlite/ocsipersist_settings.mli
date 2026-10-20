(** Settings of the SQLite backend, for programs that do not use Ocsigen
    Server's configuration file. *)

val set_db_file : string -> unit
(** [set_db_file file] sets the database file. It is required: there is no
    default. The directory of [file] must exist. Set it before
    [Ocsipersist.init ()] or the first use of a store. *)

(**/**)

val get_db_file : unit -> string
(** [get_db_file ()] is the database file. It raises [Failure] if
    {!set_db_file} was not called. *)
