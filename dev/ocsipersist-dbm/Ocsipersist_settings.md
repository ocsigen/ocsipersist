# Module `Ocsipersist_settings`

Settings of the DBM backend, for programs that do not use Ocsigen Server's configuration file. Set them before `Ocsipersist.init ()`.

```ocaml
val set_store : string -> unit
```
`set_store dir` sets the directory of the database. It is required: there is no default. ocsidbm creates `dir` if it does not exist.

```ocaml
val set_delay_loading : bool -> unit
```
`set_delay_loading true` makes `Ocsipersist.init ()` connect to ocsidbm in the background instead of waiting for it. Defaults to `false`.

```ocaml
val set_ocsidbm : string -> unit
```
`set_ocsidbm prog` sets the ocsidbm program to launch. Defaults to `"ocsidbm"`, looked up in `PATH`.

```ocaml
val set_error_log_path : string -> unit
```
`set_error_log_path file` makes ocsidbm write its errors to `file`. By default, it writes them to the standard error of the program.
