open Lwt.Syntax

let () =
  let getenv_opt v = try Some (Sys.getenv v) with Not_found -> None in
  Option.iter
    (fun s -> Ocsipersist_settings.set_port (int_of_string s))
    (getenv_opt "PGPORT");
  Option.iter Ocsipersist_settings.set_user (getenv_opt "PGUSER");
  Option.iter Ocsipersist_settings.set_database (getenv_opt "PGDATABASE");
  Option.iter Ocsipersist_settings.set_unix_domain_socket_dir
    (getenv_opt "PGHOST")

module Functorial = Ocsipersist.Functorial

module T =
  Functorial.Table
    (struct
      let name = "test_length"
    end)
    (Functorial.Column.String)
    (Functorial.Column.String)

let () = Ocsipersist.init ()

let main () =
  let* () = T.add "a" "1" in
  let* () = T.add "b" "2" in
  let* () = T.add "c" "3" in
  let* n = T.length () in
  Printf.printf "length after 3 adds: %d\n%!" n;
  let* () = T.remove "b" in
  let* n = T.length () in
  Printf.printf "length after remove: %d\n%!" n;
  (* Clean up for idempotent reruns *)
  let* () = T.remove "a" in
  let* () = T.remove "c" in
  Lwt.return_unit

let () = Lwt_main.run (main ())
