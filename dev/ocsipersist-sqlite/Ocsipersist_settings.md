# Module `Ocsipersist_settings`

Settings of the SQLite backend, for programs that do not use Ocsigen Server's configuration file.

```ocaml
val set_db_file : string -> unit
```
`set_db_file file` sets the database file. It is required: there is no default. The directory of `file` must exist. Set it before `Ocsipersist.init ()` and before the first access to the database. Creating a persistent reference or a table does not access it: its first use does.
