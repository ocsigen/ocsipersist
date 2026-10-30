let () =
  Pg_env.configure ();
  Ocsipersist.init ();
  Modify_opt_race.run ~add_during_f:true
