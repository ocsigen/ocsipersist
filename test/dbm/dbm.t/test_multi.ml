open Lwt.Syntax
open Lwt.Infix

let signal_dir = Sys.getenv "OCSIPERSIST_STORE"
let ready_file = Filename.concat signal_dir "ready"
let done_file = Filename.concat signal_dir "done"

let setup () =
  Ocsipersist_settings.set_ocsidbm (Sys.getenv "OCSIDBM");
  Ocsipersist_settings.set_store signal_dir;
  Ocsipersist_settings.set_error_log_path
    (Filename.concat signal_dir "errors.log")

module Functorial = Ocsipersist.Functorial

module T =
  Functorial.Table
    (struct
      let name = "test_multi"
    end)
    (Functorial.Column.String)
    (Functorial.Column.String)

let wait_for_file path =
  let rec aux () =
    if Sys.file_exists path then Lwt.return_unit
    else Lwt_unix.sleep 0.1 >>= fun () -> aux ()
  in
  aux ()

let touch path =
  let oc = open_out path in
  close_out oc

let writer () =
  setup ();
  Ocsipersist.init ();
  Lwt_main.run
    (let* () = T.add "x" "hello" in
     let* () = T.add "y" "world" in
     touch ready_file;
     Printf.printf "writer: data written, waiting for reader\n%!";
     let* () = wait_for_file done_file in
     Printf.printf "writer: done\n%!";
     Lwt.return_unit)

let reader () =
  setup ();
  Ocsipersist.init ();
  Lwt_main.run
    (let* () = wait_for_file ready_file in
     let* x = T.find "x" in
     let* y = T.find "y" in
     Printf.printf "reader: x=%s, y=%s\n%!" x y;
     touch done_file;
     Lwt.return_unit)

let () =
  match Sys.argv with
  | [|_; "write"|] -> writer ()
  | [|_; "read"|] -> reader ()
  | _ ->
      (* Parent: launch writer in background, then run reader *)
      let exe = Sys.argv.(0) in
      let devnull = Unix.openfile "/dev/null" [Unix.O_RDWR] 0 in
      let writer_pid =
        Unix.create_process exe [|exe; "write"|]
          devnull Unix.stdout Unix.stderr
      in
      Unix.close devnull;
      reader ();
      let _, status = Unix.waitpid [] writer_pid in
      (match status with
       | Unix.WEXITED 0 -> ()
       | _ -> Printf.eprintf "writer exited with error\n%!"; exit 1)
