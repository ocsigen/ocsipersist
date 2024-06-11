(** getting the directory from config file *)
let rec parse_global_config ((store, ocsidbm, delayloading) as d) = function
  | [] -> d
  | Xml.Element ("delayloading", [("val", ("true" | "1"))], []) :: ll ->
      parse_global_config (store, ocsidbm, true) ll
  | Xml.Element ("delayloading", [("val", ("false" | "0"))], []) :: ll ->
      parse_global_config (store, ocsidbm, false) ll
  | Xml.Element ("store", [("dir", s)], []) :: ll ->
      if store = None
      then parse_global_config (Some s, ocsidbm, delayloading) ll
      else Ocsigen_extensions.badconfig "Ocsipersist: Duplicate <store> tag"
  | Xml.Element ("ocsidbm", [("name", s)], []) :: ll ->
      if ocsidbm = None
      then parse_global_config (store, Some s, delayloading) ll
      else Ocsigen_extensions.badconfig "Ocsipersist: Duplicate <ocsidbm> tag"
  | Xml.Element (s, _, _) :: _ll -> Ocsigen_extensions.badconfig "Bad tag %s" s
  | _ ->
      Ocsigen_extensions.badconfig
        "Unexpected content inside Ocsipersist config"

let init_fun config =
  let store, ocsidbmconf, delay_loading =
    parse_global_config (None, None, false) config
  in
  Ocsipersist_settings.delay_loading := delay_loading;
  Ocsipersist_settings.error_log_path := Ocsigen_messages.error_log_path ();
  (match store with
  | None ->
      Ocsipersist_settings.directory :=
        Ocsigen_config.get_datadir () ^ "/ocsipersist"
  | Some d -> Ocsipersist_settings.directory := d);
  (match ocsidbmconf with
  | None -> ()
  | Some d -> Ocsipersist_settings.ocsidbm := d);
  Ocsipersist.init ()

let _ = Ocsigen_extensions.register ~name:"ocsipersist" ~init_fun ()
