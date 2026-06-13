open Lwt.Syntax

let () =
  let getenv v = Sys.getenv v in
  Ocsipersist_settings.set_ocsidbm (getenv "OCSIDBM");
  Ocsipersist_settings.set_store (getenv "OCSIPERSIST_STORE");
  Ocsipersist_settings.set_error_log_path
    (Filename.concat (getenv "OCSIPERSIST_STORE") "errors.log")

module Functorial = Ocsipersist.Functorial

module T =
  Functorial.Table
    (struct
      let name = "test_iter_batch"
    end)
    (Functorial.Column.String)
    (Functorial.Column.String)

let () = Ocsipersist.init ()

let main () =
  (* Add entries *)
  let* () = T.add "a" "1" in
  let* () = T.add "b" "2" in
  let* () = T.add "c" "3" in
  let* () = T.add "d" "4" in
  let* () = T.add "e" "5" in
  (* Test length *)
  let* n = T.length () in
  Printf.printf "length: %d\n%!" n;
  (* Test iter_batch: collect all batches *)
  let batches = ref 0 in
  let items = ref 0 in
  let* () =
    T.iter_batch (fun batch ->
      incr batches;
      items := !items + List.length batch;
      Lwt.return_unit)
  in
  Printf.printf "iter_batch: %d batches, %d items\n%!" !batches !items;
  Lwt.return_unit

let () = Lwt_main.run (main ())
