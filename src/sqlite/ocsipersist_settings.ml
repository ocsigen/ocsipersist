let db_file = ref None
let set_db_file s = db_file := Some s

let get_db_file () =
  match !db_file with
  | Some file -> file
  | None ->
      failwith
        "Ocsipersist: no database file. Call Ocsipersist_settings.set_db_file before using Ocsipersist, or, with Ocsigen Server's configuration file, give <database file=\"...\"/> to ocsipersist-sqlite-config."
