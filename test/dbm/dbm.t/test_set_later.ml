(* Persistent values created before Ocsipersist is configured and
   initialised, as in a module initialised before the program configures
   Ocsipersist, access the database only when first used. An access that
   fails because Ocsipersist is not initialised yet is tried again by the
   next one. *)

open Lwt.Syntax

let r = Ocsipersist.Ref.ref ~persistent:"later_ref" 0
let rj = Ocsipersist.Ref_json.ref ~persistent:"later_ref_json" [%json: int] 0

let sj =
  let* store = Ocsipersist.Store_json.open_store "later" in
  Ocsipersist.Store_json.make_persistent ~store ~name:"later" ~json:[%json: int]
    ~default:0

(* Each test reads a value and writes it back incremented. *)
let increment get set () =
  let* v = get () in
  let* () = set (v + 1) in
  Lwt.return (v + 1)

let tests =
  [ "ref", increment (fun () -> Ocsipersist.Ref.get r) (Ocsipersist.Ref.set r)
  ; ( "ref_json"
    , increment
        (fun () -> Ocsipersist.Ref_json.get rj)
        (Ocsipersist.Ref_json.set rj) )
  ; ( "store_json"
    , increment
        (fun () ->
           let* sj = sj in
           Ocsipersist.Store_json.get sj)
        (fun v ->
           let* sj = sj in
           Ocsipersist.Store_json.set sj v) ) ]

let run_tests () =
  List.iter
    (fun (name, test) ->
       match Lwt_main.run (test ()) with
       | v -> Printf.printf "  %s: %d\n%!" name v
       | exception Failure msg -> Printf.printf "  %s: %s\n%!" name msg)
    tests

(* With argument [early], the values are first used before Ocsipersist is
   initialised. *)
let () =
  if Array.mem "early" Sys.argv
  then (
    print_endline "Before Ocsipersist.init:";
    run_tests ());
  let store = Sys.getenv "OCSIPERSIST_STORE" in
  Ocsipersist_settings.set_ocsidbm (Sys.getenv "OCSIDBM");
  Ocsipersist_settings.set_store store;
  Ocsipersist_settings.set_error_log_path (Filename.concat store "errors.log");
  Ocsipersist.init ();
  print_endline "After Ocsipersist.init:";
  run_tests ()
