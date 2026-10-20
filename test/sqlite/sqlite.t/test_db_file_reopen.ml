(* When the database file changes and the new file cannot be opened, the
   connection to the old file, now closed, is dropped: a later access with
   the old file opens it again instead of using the closed connection. *)

let access name =
  match Lwt_main.run (Ocsipersist.Store.open_store "reopen") with
  | (_ : Ocsipersist.store) -> Printf.printf "%s: ok\n%!" name
  | exception Sqlite3.Error _ -> Printf.printf "%s: Sqlite3.Error\n%!" name

let () =
  Ocsipersist_settings.set_db_file "reopen.db";
  access "first file";
  Ocsipersist_settings.set_db_file "missing-dir/reopen.db";
  access "file that cannot be opened";
  Ocsipersist_settings.set_db_file "reopen.db";
  access "first file again"
