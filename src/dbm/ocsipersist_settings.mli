val set_store : string -> unit
val set_delay_loading : bool -> unit
val set_ocsidbm : string -> unit
val set_error_log_path : string -> unit

(**/**)

val directory : string ref
val ocsidbm : string ref
val delay_loading : bool ref

val error_log_path : string option ref
(** The file where ocsidbm writes its errors. With [None], it writes them to
    the standard error of the server. *)

val inch : Lwt_io.input_channel Lwt.t ref
val outch : Lwt_io.output_channel Lwt.t ref
