This program will print an incremented number each time:

  $ dune exec -- ./test.exe
  1
  $ dune exec -- ./test.exe
  2
  $ dune exec -- ./test.exe
  3

Test Functorial interface (length, iter_batch):

  $ dune exec -- ./test_functorial.exe
  length: 5
  iter_batch: 1 batches, 5 items
  iter_batch ~count:3: 3 items
