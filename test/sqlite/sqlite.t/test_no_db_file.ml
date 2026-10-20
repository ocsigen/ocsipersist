(* In library mode, there is no default database file: until
   [Ocsipersist_settings.set_db_file] is called, [Ocsipersist.init] and the
   first use of a store fail with a clear error. *)

let r = Ocsipersist.Ref.ref ~persistent:"r" 0

let report name f =
  match f () with
  | () -> Printf.printf "%s: no error\n%!" name
  | exception Failure msg -> Printf.printf "%s: %s\n%!" name msg

let () =
  report "init" Ocsipersist.init;
  report "get" (fun () -> ignore (Lwt_main.run (Ocsipersist.Ref.get r) : int))
