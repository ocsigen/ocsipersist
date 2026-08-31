
# Ocsipersist

Ocsipersist is a collection of libraries defining a unified frontend for a number of key/value storage implementations. It is used pervasively in Ocsigen/Eliom to handle sessions and persistent references, but can be used as a standalone library or as an extension for Ocsigen Server.

Three backends currently exist:

- [SQLite](../ocsipersist-sqlite/index.html)
- [DBM](../ocsipersist-dbm/index.html)
- [PostgreSQL](../ocsipersist-pgsql/index.html)
You choose a backend by installing the corresponding package (`ocsipersist-sqlite`, `ocsipersist-dbm` or `ocsipersist-pgsql`); the unified frontend is the same in every case.


## API reference

The storage frontend is documented in module [`Ocsipersist`](./Ocsipersist.md). It defines several interfaces:

- `Ref` — the simplest to use: persistent references;
- `Store` — a lower-level interface for persistent values;
- `Polymorphic` — a polymorphic table (using `Marshal`);
- `Functorial` — a type-safe table built from a value description.
See [`Ocsipersist`](./Ocsipersist.md) for the full frontend, and [`Ocsipersist_lib`](./../ocsipersist-lib/Ocsipersist_lib.md) for the backend-independent signatures and functors shared by every backend.


## Backends and configuration

Each backend ships an `Ocsipersist_settings` library to configure storage from OCaml, and an optional `ocsipersist-*-config` package to configure it from Ocsigen Server's configuration file:

- SQLite: [usage](../ocsipersist-sqlite/index.html) · [server config](../ocsipersist-sqlite-config/Ocsipersist_config/index.html)
- DBM: [usage](../ocsipersist-dbm/index.html) · [server config](../ocsipersist-dbm-config/Ocsipersist_config/index.html)
- PostgreSQL: [usage](../ocsipersist-pgsql/index.html) · [server config](../ocsipersist-pgsql-config/Ocsipersist_config/index.html)