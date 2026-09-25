(* A stored value that the codec cannot decode (here: the type changed
   between two openings of the same name) must not break [make_persistent];
   [get] reports it with [Decoding_error] and [set] can overwrite it. *)

open Lwt.Syntax

[@@@warning "-39"]
(* ppx_deriving_json generates unused rec flags *)

type user = {name : string; age : int} [@@deriving json]

let () = Ocsipersist.init ()

let main () =
  let* store = Ocsipersist.Store_json.open_store "test_decoding_error" in
  let* pv_int =
    Ocsipersist.Store_json.make_persistent ~store ~name:"v" ~json:[%json: int]
      ~default:0
  in
  let* () = Ocsipersist.Store_json.set pv_int 42 in
  (* Reopening with an incompatible codec succeeds. *)
  let* pv_user =
    Ocsipersist.Store_json.make_persistent ~store ~name:"v" ~json:[%json: user]
      ~default:{name = "default"; age = 0}
  in
  print_endline "make_persistent: ok";
  (* Reading the stored int as a user fails with Decoding_error. *)
  let* () =
    Lwt.catch
      (fun () ->
         let* _ = Ocsipersist.Store_json.get pv_user in
         print_endline "get: unexpected success";
         Lwt.return_unit)
      (function
        | Ocsipersist.Decoding_error _ ->
            print_endline "get: Decoding_error";
            Lwt.return_unit
        | e -> Lwt.reraise e)
  in
  (* The caller can recover by overwriting the unreadable value. *)
  let* () = Ocsipersist.Store_json.set pv_user {name = "Carol"; age = 1} in
  let* u = Ocsipersist.Store_json.get pv_user in
  Printf.printf "after set: name=%s, age=%d\n%!" u.name u.age;
  Lwt.return_unit

let () = Lwt_main.run (main ())
