let directory, ocsidbm = (ref "ocsipersist-store", ref "ocsidbm")
let inch = ref (raise (Failure "Ocsipersist not initialised"))
let outch = ref (raise (Failure "Ocsipersist not initialised"))
let delay_loading = ref false
let error_log_path = ref "ocsipersist-errors"
let set_error_log_path s = error_log_path := s
let set_store s = directory := s
let set_ocsidbm s = ocsidbm := s
let set_delay_loading b = delay_loading := b
