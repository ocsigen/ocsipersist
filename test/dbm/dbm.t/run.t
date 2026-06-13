Test Functorial iter_batch with DBM backend and ocsidbm process.

Set up environment:

  $ export OCSIDBM=$(which ocsidbm)
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
