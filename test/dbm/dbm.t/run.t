Test Functorial iter_batch with DBM backend and ocsidbm process.

Set up environment:

  $ export OCSIDBM=$(which ocsidbm)
  $ export OCSIPERSIST_STORE=$(mktemp -d)

Run the test:

  $ dune exec -- ./test.exe
  length: 5
  iter_batch: 1 batches, 5 items
