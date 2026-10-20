(* An access uses the database file set when the access is made, even if
   the file is changed before the access runs in its worker thread. *)

let () =
  Ocsipersist_settings.set_db_file "first.db";
  let store = Ocsipersist.Store.open_store "change" in
  Ocsipersist_settings.set_db_file "second.db";
  ignore (Lwt_main.run store : Ocsipersist.store)
