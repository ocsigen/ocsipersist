# ocsipersist-sqlite

SQLite backend for Ocsipersist. For the API documentation see OPAM package [ocsipersist](../ocsipersist/Ocsipersist/index.html). This page describes how to configure the SQLite backend.

## Using as a library

If you are not using Ocsigen Server's configuration file, add library `ocsipersist-sqlite.settings` in your Dune file, and use module [`Ocsipersist_settings`](./Ocsipersist_settings.md) to configure the storage file. The database file is required: there is no default. Set it before `Ocsipersist.init ()` or the first use of a store, otherwise they fail with an error asking for it. The directory of the file must exist.

```ocaml
let () =
  Ocsipersist_settings.set_db_file "data/ocsidb";
  Ocsipersist.init ()
```

## Using with Ocsigen Server: ocsipersist-sqlite-config

If you want to configure Ocsipersist-sqlite from Ocsigen Server's configuration file, use package [ocsipersist-sqlite-config](../ocsipersist-sqlite-config/Ocsipersist_config/index.html).
