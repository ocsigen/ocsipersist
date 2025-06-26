let r = Ocsipersist.Ref.ref ~persistent:"r" 0

let main () =
  let v = Ocsipersist.Ref.get r in
  let v = v + 1 in
  let () = Ocsipersist.Ref.set r v in
  Printf.printf "%d\n%!" v

let () =
  Eio_main.run (fun env ->
      Ocsipersist.init ~env;
      main ()
    )
