# ocsipersist-dbm

DBM backend for Ocsipersist. For the API documentation see OPAM package [ocsipersist](../ocsipersist/Ocsipersist/index.html). This page describes how to configure the DBM backend. The DBM backend uses a server process `ocsidbm`.

## Using as a library

If you are not using Ocsigen Server's configuration file, add library `ocsipersist-dbm.settings` in your Dune file, and use module [`Ocsipersist_settings`](./Ocsipersist_settings.md) to configure the storage directory. The directory is required: there is no default. Set it before `Ocsipersist.init ()`, otherwise it fails with an error asking for it. By default, ocsidbm writes its errors to the standard error of the program; [`Ocsipersist_settings.set_error_log_path`](./Ocsipersist_settings.md#val-set_error_log_path) sends them to a file instead.

```ocaml
let () =
  Ocsipersist_settings.set_store "data/store";
  Ocsipersist.init ()
```

## Using with Ocsigen Server: ocsipersist-dbm-config

If you want to configure Ocsipersist-dbm from Ocsigen Server's configuration file, use package [ocsipersist-dbm-config](../ocsipersist-dbm-config/Ocsipersist_config/index.html).
