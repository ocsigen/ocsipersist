let directory, ocsidbm = ref None, ref "ocsidbm"
let inch = ref (Lwt.fail (Failure "Ocsipersist not initialised"))
let outch = ref (Lwt.fail (Failure "Ocsipersist not initialised"))
let delay_loading = ref false
let error_log_path = ref None
let set_error_log_path s = error_log_path := Some s
let set_store s = directory := Some s
let set_ocsidbm s = ocsidbm := s
let set_delay_loading b = delay_loading := b

let get_store () =
  match !directory with
  | Some d -> d
  | None ->
      failwith
        "Ocsipersist: no store directory. Call Ocsipersist_settings.set_store before Ocsipersist.init, or, with Ocsigen Server's configuration file, give <store dir=\"...\"/> to ocsipersist-dbm-config."
