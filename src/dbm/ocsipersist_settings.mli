(** Settings of the DBM backend, for programs that do not use Ocsigen
    Server's configuration file. Set them before [Ocsipersist.init ()]. *)

val set_store : string -> unit
(** [set_store dir] sets the directory of the database. It is required:
    there is no default. ocsidbm creates [dir] if it does not exist. *)

val set_delay_loading : bool -> unit
(** [set_delay_loading true] makes [Ocsipersist.init ()] connect to ocsidbm
    in the background instead of waiting for it. Defaults to [false]. *)

val set_ocsidbm : string -> unit
(** [set_ocsidbm prog] sets the ocsidbm program to launch. Defaults to
    ["ocsidbm"], looked up in [PATH]. *)

val set_error_log_path : string -> unit
(** [set_error_log_path file] makes ocsidbm write its errors to [file]. By
    default, it writes them to the standard error of the program. *)

(**/**)

val get_store : unit -> string
(** [get_store ()] is the directory of the database. It raises [Failure] if
    {!set_store} was not called. *)

val ocsidbm : string ref
val delay_loading : bool ref

val error_log_path : string option ref
(** The file where ocsidbm writes its errors. With [None], it writes them to
    the standard error of the server. *)

val inch : Lwt_io.input_channel Lwt.t ref
val outch : Lwt_io.output_channel Lwt.t ref
