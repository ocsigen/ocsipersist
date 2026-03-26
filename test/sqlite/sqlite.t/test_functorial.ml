open Lwt.Syntax

module Functorial = Ocsipersist.Functorial

module T =
  Functorial.Table
    (struct
      let name = "test_functorial"
    end)
    (Functorial.Column.String)
    (Functorial.Column.String)

let () = Ocsipersist.init ()

let main () =
  (* Clean slate *)
  let* () = T.iter (fun k _ -> T.remove k) in
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
  (* Test iter_batch with count *)
  let items_limited = ref 0 in
  let* () =
    T.iter_batch ~count:3L (fun batch ->
      items_limited := !items_limited + List.length batch;
      Lwt.return_unit)
  in
  Printf.printf "iter_batch ~count:3: %d items\n%!" !items_limited;
  (* Clean up *)
  let* () = T.iter (fun k _ -> T.remove k) in
  Lwt.return_unit

let () = Lwt_main.run (main ())
