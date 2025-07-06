(lang dune 2.8)
(name ocsipersist)

(generate_opam_files true)

(maintainers "Jan Rochel <jan@besport.com>")
(authors "Ocsigen team <dev@ocsigen.org>")
(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
(source (github ocsigen/ocsipersist))

(package
 (name ocsipersist)
 (synopsis "Persistent key-value storage for OCaml using multiple backends")
 (description "This is an virtual library defining a unified frontend for a number of key-value storage implementations. Implementations of the following backends currently exist: DBM, PostgreSQL, SQLite.")
 (depends
  (ocsipersist-lib (= :version))
  (lwt (>= 4.2.0)))
 (conflicts
  (ocsipersist-dbm (<> :version))
  (ocsipersist-pgsql (<> :version))
  (ocsipersist-sqlite (<> :version))))

(package
 (name ocsipersist-dbm)
 (synopsis "Persistent key/value storage for OCaml using DBM")
 (description "This library provides a DBM backend for the unified key/value storage frontend as defined in the ocsipersist package.")
 (depends
  (ocsipersist (= :version))
  (lwt (>= 4.2.0))
  logs
  dbm))

(package
 (name ocsipersist-dbm-config)
 (synopsis "Ocsigen Server configuration file extension for ocsipersist-dbm")
 (description "Load this package from Ocsigen Server's configuration file if you want to use the DBM storage backend.")
 (depends
  (ocsipersist-dbm (= :version))
  (ocsigenserver (>= 3.0.0))
  xml-light))

(package
 (name ocsipersist-lib)
 (synopsis "Persistent key/value storage for OCaml - support library")
 (description "This library defines signatures and auxiliary tools for defining backends for the Ocsipersist frontent. Ocsipersist is used pervasively in Eliom/Ocsigen to handle sessions and references. It can be used as an extension for ocsigenserver or as a library. Implementations of the following backends currently exist: DBM, PostgreSQL, SQLite.")
 (depends
  (lwt (>= 4.2.0))))

(package
 (name ocsipersist-pgsql)
 (synopsis "Persistent key/value storage for OCaml using PostgreSQL")
 (description "This library provides a PostgreSQL backend for the unified key/value storage frontend as defined in the ocsipersist package.")
 (depends
  (ocaml (>= 4.08))
  (ocsipersist (= :version))
  (lwt (>= 4.2.0))
  logs
  pgocaml))

(package
 (name ocsipersist-pgsql-config)
 (synopsis "Ocsigen Server configuration file extension for ocsipersist-pgsql")
 (description "Load this package from Ocsigen Server's configuration file if you want to use the PostgreSQL storage backend.")
 (depends
  (ocsipersist-pgsql (= :version))
  (ocsigenserver (>= 3.0.0))
  xml-light))

(package
 (name ocsipersist-sqlite)
 (synopsis "Persistent key/value storage for OCaml using SQLite")
 (description "This library provides a SQLite backend for the unified key/value storage frontend as defined in the ocsipersist package.")
 (depends
  (lwt (>= 4.2.0))
  logs
  ocsipersist
  sqlite3))

(package
 (name ocsipersist-sqlite-config)
 (synopsis "Ocsigen Server configuration file extension for ocsipersist-sqlite")
 (description "Load this package from Ocsigen Server's configuration file if you want to use the SQLite storage backend.")
 (depends
  (ocsipersist-sqlite (= :version))
  (ocsigenserver (>= 3.0.0))
  xml-light))
