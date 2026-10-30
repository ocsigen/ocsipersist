(* Test of [modify_opt] shared by the backends. A cram test only sees its own
   directory, so each one copies this file before building. The test program
   of a backend configures it, then calls [run]. *)

open Lwt.Syntax

module T =
  Ocsipersist.Functorial.Table
    (struct
      let name = "test_modify_opt"
    end)
    (Ocsipersist.Functorial.Column.String)
    (Ocsipersist.Functorial.Column.String)

let processes = 2
let threads = 4
let increments = 50

let incr = function
  | None -> Some "1"
  | Some n -> Some (string_of_int (int_of_string n + 1))

let find_opt key =
  Lwt.catch
    (fun () ->
       let+ v = T.find key in
       Some v)
    (function Not_found -> Lwt.return_none | e -> Lwt.fail e)

let show key =
  let+ v = find_opt key in
  Printf.printf "%s: %s\n%!" key (Option.value v ~default:"absent")

let single_process () =
  let* () = T.remove "counter" in
  let* () = T.remove "created" in
  let* () = T.add "kept" "v" in
  let* () =
    T.modify_opt "created" (function None -> Some "new" | Some _ -> None)
  in
  let* () = show "created" in
  let* () = T.modify_opt "created" (fun _ -> None) in
  let* () = show "created" in
  let* () =
    Lwt.catch
      (fun () -> T.modify_opt "kept" (fun _ -> failwith "f failed"))
      (function
        | Failure msg -> Lwt.return (Printf.printf "modify_opt: %s\n%!" msg)
        | e -> Lwt.fail e)
  in
  show "kept"

(* Each process runs [threads] threads, each incrementing the counter
   [increments] times. *)
let worker () =
  let rec loop i =
    if i = 0
    then Lwt.return_unit
    else
      let* () = T.modify_opt "counter" incr in
      loop (i - 1)
  in
  Lwt.join (List.init threads (fun _ -> loop increments))

(* [Lwt_main.run] installs a SIGCHLD handler, which can interrupt
   [waitpid]. *)
let rec waitpid_restart pid =
  try snd (Unix.waitpid [] pid)
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_restart pid

let run () =
  match Sys.argv with
  | [|_; "worker"|] -> Lwt_main.run (worker ())
  | _ ->
      (* The table is created before the workers start. *)
      Lwt_main.run (single_process ());
      let exe = Sys.executable_name in
      let pids =
        List.init processes (fun _ ->
          Unix.create_process exe [|exe; "worker"|] Unix.stdin Unix.stdout
            Unix.stderr)
      in
      List.iter
        (fun pid ->
           match waitpid_restart pid with
           | Unix.WEXITED 0 -> ()
           | _ -> print_endline "a worker failed")
        pids;
      Printf.printf "expected counter: %d\n" (processes * threads * increments);
      Lwt_main.run (show "counter")
