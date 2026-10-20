Test Functorial iter_batch with DBM backend and ocsidbm process.

Set up environment:

  $ export OCSIDBM=$(command -v ocsidbm)
  $ export OCSIPERSIST_STORE=$(mktemp -d)

Run the test:

  $ dune exec -- ./test.exe
  length: 5
  iter_batch: 1 batches, 5 items

Persistent values created before Ocsipersist is configured and initialised,
as in a module initialised before the configuration, access the database
at their first use, not when they are created:

  $ dune exec -- ./test_set_later.exe
  After Ocsipersist.init:
    ref: 1
    ref_json: 1
    store_json: 1

An access that fails because Ocsipersist is not initialised yet is tried
again by the next one:

  $ dune exec -- ./test_set_later.exe early
  Before Ocsipersist.init:
    ref: Ocsipersist not initialised
    ref_json: Ocsipersist not initialised
    store_json: Ocsipersist not initialised
  After Ocsipersist.init:
    ref: 2
    ref_json: 2
    store_json: 2

Test two separate processes accessing the same ocsidbm simultaneously.
The test_multi executable forks a writer subprocess, then runs a reader
in the parent. Both connect to the same ocsidbm and share data.

  $ OCSIPERSIST_STORE=$(mktemp -d) dune exec -- ./test_multi.exe
  writer: data written, waiting for reader
  reader: x=hello, y=world
  writer: done

In library mode, the store directory must be set: there is no default.
Without it, Ocsipersist.init fails with a clear error, also when the
connection to ocsidbm is delayed:

  $ dune exec -- ./test_no_store.exe
  init: Ocsipersist: no store directory. Call Ocsipersist_settings.set_store before Ocsipersist.init, or, with Ocsigen Server's configuration file, give <store dir="..."/> to ocsipersist-dbm-config.
  init with delay_loading: Ocsipersist: no store directory. Call Ocsipersist_settings.set_store before Ocsipersist.init, or, with Ocsigen Server's configuration file, give <store dir="..."/> to ocsipersist-dbm-config.
