(* In library mode, there is no default store directory:
   [Ocsipersist.init] fails with a clear error until
   [Ocsipersist_settings.set_store] is called, even when the connection to
   ocsidbm is delayed. *)

let report name =
  match Ocsipersist.init () with
  | () -> Printf.printf "%s: no error\n%!" name
  | exception Failure msg -> Printf.printf "%s: %s\n%!" name msg

let () =
  report "init";
  Ocsipersist_settings.set_delay_loading true;
  report "init with delay_loading"
