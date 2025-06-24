let parse_global_config = function
  | [] -> None
  | [Xml.Element ("database", [("file", s)], [])] -> Some s
  | _ ->
      raise
        (Ocsigen_extensions.Error_in_config_file
           "Unexpected content inside Ocsipersist config")

let init config =
  Ocsipersist_settings.db_file := Ocsigen_config.get_datadir () ^ "/ocsidb";
  (match parse_global_config config with
  | None -> ()
  | Some d -> Ocsipersist_settings.db_file := d);
  try Ocsipersist.init ()
  with e ->
    Ocsigen_messages.errlog
      (Printf.sprintf
         "Error opening database file '%s' when registering Ocsipersist. Check that the directory exists, and that Ocsigen has enough rights"
         !Ocsipersist_settings.db_file);
    raise e

let () = Ocsigen_extensions.register ~name:"ocsipersist" ~init_fun:init ()
