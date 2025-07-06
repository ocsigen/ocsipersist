open Lwt.Syntax

let () = Ocsipersist.init ()
let r = Ocsipersist.Ref.ref ~persistent:"r" 0

let main () =
  let* v = Ocsipersist.Ref.get r in
  let v = v + 1 in
  let* () = Ocsipersist.Ref.set r v in
  Printf.printf "%d\n%!" v;
  Lwt.return_unit

let () = Lwt_main.run (main ())
