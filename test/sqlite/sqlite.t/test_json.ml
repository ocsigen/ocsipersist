open Lwt.Syntax

[@@@warning "-39"]
(* ppx_deriving_json generates unused rec flags *)

type user = {name : string; age : int} [@@deriving json]

let () = Ocsipersist.init ()

let main () =
  (* Test Ref_json *)
  let r =
    Ocsipersist.Ref_json.ref ~persistent:"json_ref" [%json: int] 0
  in
  let* v = Ocsipersist.Ref_json.get r in
  let v = v + 1 in
  let* () = Ocsipersist.Ref_json.set r v in
  Printf.printf "ref_json: %d\n%!" v;
  (* Test Store_json with a record type *)
  let* store = Ocsipersist.Store_json.open_store "test_json" in
  let* pv =
    Ocsipersist.Store_json.make_persistent ~store ~name:"alice"
      ~json:[%json: user] ~default:{name = "Alice"; age = 30}
  in
  let* u = Ocsipersist.Store_json.get pv in
  Printf.printf "store_json: name=%s, age=%d\n%!" u.name u.age;
  let* () =
    Ocsipersist.Store_json.set pv {name = "Alice"; age = u.age + 1}
  in
  let* u = Ocsipersist.Store_json.get pv in
  Printf.printf "store_json after update: name=%s, age=%d\n%!" u.name u.age;
  (* Test Column.Json via Functorial *)
  let module T =
    Ocsipersist.Functorial.Table
      (struct
        let name = "test_json_table"
      end)
      (Ocsipersist.Functorial.Column.String)
      (Ocsipersist.Functorial.Column.Json (struct
           type t = user

           let t = user_json
         end))
  in
  let* () = T.add "bob" {name = "Bob"; age = 25} in
  let* bob = T.find "bob" in
  Printf.printf "functorial_json: name=%s, age=%d\n%!" bob.name bob.age;
  let* () = T.remove "bob" in
  Lwt.return_unit

let () = Lwt_main.run (main ())
