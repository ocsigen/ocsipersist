let () =
  Ocsipersist_settings.set_db_file "modify_opt.db";
  Modify_opt_race.run ()
