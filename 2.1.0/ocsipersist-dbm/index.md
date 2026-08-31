
# ocsipersist-dbm

DBM backend for Ocsipersist. For the API documentation see OPAM package [ocsipersist](../ocsipersist/Ocsipersist/index.html). This page describes how to configure the DBM backend. The DBM backend uses a server process `ocsidbm`.


## Using as a library

If you are not using Ocsigen Server's configuration file, add library `ocsipersist-dbm.settings` in your Dune file, and use module [`Ocsipersist_settings`](./Ocsipersist_settings.md) to configure the storage file.


## Using with Ocsigen Server: ocsipersist-dbm-config

If you want to configure Ocsipersist-dbm from Ocsigen Server's configuration file, use package [ocsipersist-dbm-config](../ocsipersist-dbm-config/Ocsipersist_config/index.html).
