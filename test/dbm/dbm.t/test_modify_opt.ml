let () =
  let store = Sys.getenv "OCSIPERSIST_STORE" in
  Ocsipersist_settings.set_ocsidbm (Sys.getenv "OCSIDBM");
  Ocsipersist_settings.set_store store;
  Ocsipersist_settings.set_error_log_path (Filename.concat store "errors.log");
  Ocsipersist.init ();
  Modify_opt_race.run ()
