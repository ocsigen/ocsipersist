let host = ref None
let port = ref None
let user = ref None
let password = ref None
let database = ref "ocsipersist"
let unix_domain_socket_dir = ref None
let size_conn_pool = ref 16
let set_host (s : string) = host := Some s
let set_port (s : int) = port := Some s
let set_user (s : string) = user := Some s
let set_password (s : string) = password := Some s
let set_database (s : string) = database := s
let set_unix_domain_socket_dir (s : string) = unix_domain_socket_dir := Some s
let set_connexion_pool_size (s : int) = size_conn_pool := s
