open Eio.Std

val set_store : string -> unit
val set_delay_loading : bool -> unit
val set_ocsidbm : string -> unit
val set_error_log_path : string -> unit

(**/**)

val directory : string ref
val ocsidbm : string ref
val delay_loading : bool ref
val error_log_path : string ref
val inch : Eio.Buf_read.t Promise.t ref
val outch : Eio.Buf_write.t Promise.t ref
