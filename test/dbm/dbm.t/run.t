Test Functorial iter_batch with DBM backend and ocsidbm process.

Set up environment:

  $ export OCSIDBM=$(command -v ocsidbm)
  $ export OCSIPERSIST_STORE=$(mktemp -d)

Run the test:

  $ dune exec -- ./test.exe
  length: 5
  iter_batch: 1 batches, 5 items

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
