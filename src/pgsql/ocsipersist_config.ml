let section = Lwt_log.Section.make "ocsigen:ocsipersist:pgsql:config"
let () = Lwt_log.ign_info ~section "Init for Ocsigen Server config file"

let parse_global_config = function
  | [] -> ()
  | [Xml.Element ("database", attrs, [])] ->
      let parse_attr = function
        | "host", h -> Ocsipersist_settings.host := Some h
        | "port", p -> (
          try Ocsipersist_settings.port := Some (int_of_string p)
          with Failure _ ->
            raise
            @@ Ocsigen_extensions.Error_in_config_file "port is not an integer")
        | "user", u -> Ocsipersist_settings.user := Some u
        | "password", pw -> Ocsipersist_settings.password := Some pw
        | "database", db -> Ocsipersist_settings.database := db
        | "unix_domain_socket_dir", udsd ->
            Ocsipersist_settings.unix_domain_socket_dir := Some udsd
        | "size_conn_pool", scp -> (
          try Ocsipersist_settings.size_conn_pool := int_of_string scp
          with Failure _ ->
            raise
            @@ Ocsigen_extensions.Error_in_config_file
                 "size_conn_pool is not an integer")
        | _ ->
            raise
            @@ Ocsigen_extensions.Error_in_config_file
                 "Unexpected attribute for <database> in Ocsipersist config"
      in
      ignore @@ List.map parse_attr attrs;
      ()
  | _ ->
      raise
      @@ Ocsigen_extensions.Error_in_config_file
           "Unexpected content inside Ocsipersist config"

let init_fun config = parse_global_config config; Ocsipersist.init ()
let () = Ocsigen_extensions.register ~name:"ocsipersist" ~init_fun ()
