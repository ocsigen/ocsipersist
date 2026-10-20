let parse_global_config = function
  | [Xml.Element ("database", [("file", s)], [])] -> s
  | [] ->
      Ocsigen.Extensions.badconfig
        "Ocsipersist: <database file=\"...\"/> is missing. It gives the database file (Ocsigen Server's <datadir> is no longer used)."
  | _ ->
      raise
        (Ocsigen.Extensions.Error_in_config_file
           "Unexpected content inside Ocsipersist config")

let init config =
  let file = parse_global_config config in
  Ocsipersist_settings.set_db_file file;
  try Ocsipersist.init ()
  with e ->
    Ocsigen.Messages.errlog
      (Printf.sprintf
         "Error opening database file '%s' when registering Ocsipersist. Check that the directory exists, and that Ocsigen has enough rights"
         file);
    raise e

let () = Ocsigen.Extensions.register ~name:"ocsipersist" ~init_fun:init ()
