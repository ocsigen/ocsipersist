(* Connection settings from the environment set by run.t. *)
let configure () =
  let getenv_opt v = try Some (Sys.getenv v) with Not_found -> None in
  Option.iter
    (fun s -> Ocsipersist_settings.set_port (int_of_string s))
    (getenv_opt "PGPORT");
  Option.iter Ocsipersist_settings.set_user (getenv_opt "PGUSER");
  Option.iter Ocsipersist_settings.set_database (getenv_opt "PGDATABASE");
  Option.iter Ocsipersist_settings.set_unix_domain_socket_dir
    (getenv_opt "PGHOST")
