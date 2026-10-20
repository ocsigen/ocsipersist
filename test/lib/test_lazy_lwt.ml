(* [Lazy_lwt] runs its computation at the first [force], shares it between
   concurrent callers, keeps a success and tries again after a failure. *)

module Lazy_lwt = Ocsipersist_lib.Lazy_lwt

(* A lazy promise whose computations are resolved by the test, through the
   resolvers pushed in [pending]. *)
let make () =
  let calls = ref 0 and pending = Queue.create () in
  let l =
    Lazy_lwt.make (fun () ->
      incr calls;
      let p, u = Lwt.task () in
      Queue.push u pending; p)
  in
  l, calls, pending

let show calls name p =
  Printf.printf "%s: %s (computations: %d)\n" name
    (match Lwt.state p with
    | Lwt.Return v -> Printf.sprintf "fulfilled with %d" v
    | Lwt.Fail Lwt.Canceled -> "cancelled"
    | Lwt.Fail e -> "rejected with " ^ Printexc.to_string e
    | Lwt.Sleep -> "pending")
    !calls

let () =
  print_endline "Sharing, retry after a failure, caching of a success:";
  let l, calls, pending = make () in
  let show = show calls in
  Printf.printf "make: (computations: %d)\n" !calls;
  let p1 = Lazy_lwt.force l in
  let p2 = Lazy_lwt.force l in
  show "first force" p1;
  show "concurrent force" p2;
  Lwt.wakeup_exn (Queue.pop pending) (Failure "not configured");
  show "first force" p1;
  show "concurrent force" p2;
  let p3 = Lazy_lwt.force l in
  show "force after the failure" p3;
  Lwt.wakeup (Queue.pop pending) 42;
  show "force after the failure" p3;
  show "force after the success" (Lazy_lwt.force l)

let () =
  print_endline "An exception raised by the computation is retried too:";
  let calls = ref 0 in
  let l =
    Lazy_lwt.make (fun () ->
      incr calls;
      if !calls = 1 then failwith "not configured" else Lwt.return 1)
  in
  show calls "first force" (Lazy_lwt.force l);
  show calls "second force" (Lazy_lwt.force l)

let () =
  print_endline "Cancelling one caller does not cancel the others:";
  let l, calls, pending = make () in
  let show = show calls in
  let p1 = Lazy_lwt.force l in
  let p2 = Lazy_lwt.force l in
  Lwt.cancel p1;
  show "cancelled force" p1;
  show "other force" p2;
  Lwt.wakeup (Queue.pop pending) 7;
  show "other force" p2

let () =
  print_endline "A force from the failure handler of a caller runs it again:";
  let l, calls, pending = make () in
  let p = Lwt.catch (fun () -> Lazy_lwt.force l) (fun _ -> Lazy_lwt.force l) in
  Lwt.wakeup_exn (Queue.pop pending) (Failure "not configured");
  show calls "force from the handler" p;
  Lwt.wakeup (Queue.pop pending) 3;
  show calls "force from the handler" p
