(* A server whose configuration file, given as argument, initialises the
   statically linked ocsipersist-dbm-config extension. *)

let () =
  Logs.set_reporter Ocsigen.Messages.stdio_reporter;
  match Sys.argv with
  | [| _; file |] ->
      Ocsigen.Server.exec (Ocsigen.Parseconfig.parse_config ~file ())
  | _ -> invalid_arg "usage: test.exe CONFIG_FILE"
