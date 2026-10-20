(* Persistent values created before the database file is set, as in a module
   initialised before the program configures Ocsipersist, access the
   database only when first used. An access that fails because the file is
   not set yet is tried again by the next one. *)

open Lwt.Syntax

let r = Ocsipersist.Ref.ref ~persistent:"later_ref" 0
let rj = Ocsipersist.Ref_json.ref ~persistent:"later_ref_json" [%json: int] 0

let sj =
  let* store = Ocsipersist.Store_json.open_store "later" in
  Ocsipersist.Store_json.make_persistent ~store ~name:"later" ~json:[%json: int]
    ~default:0

module T =
  Ocsipersist.Functorial.Table
    (struct
      let name = "later_table"
    end)
    (Ocsipersist.Functorial.Column.String)
    (Ocsipersist.Functorial.Column.Float)

let pt : int Ocsipersist.table Lwt.t =
  Ocsipersist.Polymorphic.open_table "later_polymorphic"

(* Each test reads a value and writes it back incremented. *)
let increment get set () =
  let* v = get () in
  let* () = set (v + 1) in
  Lwt.return (v + 1)

let or_zero get () =
  Lwt.catch get (function Not_found -> Lwt.return 0 | e -> Lwt.reraise e)

let tests =
  [ ( "functorial"
    , increment
        (or_zero (fun () ->
           let* v = T.find "v" in
           Lwt.return (int_of_float v)))
        (fun v -> T.add "v" (float_of_int v)) )
  ; ( "polymorphic"
    , increment
        (or_zero (fun () ->
           let* pt = pt in
           Ocsipersist.Polymorphic.find pt "v"))
        (fun v ->
           let* pt = pt in
           Ocsipersist.Polymorphic.add pt "v" v) )
  ; "ref", increment (fun () -> Ocsipersist.Ref.get r) (Ocsipersist.Ref.set r)
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
       | exception Failure msg ->
           (* Only the first sentence of the error asking for the file. *)
           Printf.printf "  %s: %s\n%!" name
             (List.hd (String.split_on_char '.' msg)))
    tests

(* With argument [early], the values are first used before the file is
   set. *)
let () =
  if Array.mem "early" Sys.argv
  then (
    print_endline "Before set_db_file:";
    run_tests ());
  Ocsipersist_settings.set_db_file "ocsidb";
  print_endline "After set_db_file:";
  run_tests ()
