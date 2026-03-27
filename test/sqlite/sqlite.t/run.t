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

Test JSON serialisation (Ref_json, Store_json, Column.Json):

  $ dune exec -- ./test_json.exe
  ref_json: 1
  store_json: name=Alice, age=30
  store_json after update: name=Alice, age=31
  functorial_json: name=Bob, age=25
  $ dune exec -- ./test_json.exe
  ref_json: 2
  store_json: name=Alice, age=31
  store_json after update: name=Alice, age=32
  functorial_json: name=Bob, age=25
