val set_host : string -> unit
val set_port : int -> unit
val set_user : string -> unit
val set_password : string -> unit
val set_database : string -> unit
val set_unix_domain_socket_dir : string -> unit
val set_connexion_pool_size : int -> unit

(**/**)

val host : string option ref
val port : int option ref
val user : string option ref
val password : string option ref
val database : string ref
val unix_domain_socket_dir : string option ref
val size_conn_pool : int ref
