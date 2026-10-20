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

An unreadable stored value (type changed) does not break make_persistent;
get reports it with Decoding_error and set can overwrite it:

  $ dune exec -- ./test_decoding_error.exe
  make_persistent: ok
  get: Decoding_error
  after set: name=Carol, age=1

In library mode, the database file must be set: there is no default.
Without it, Ocsipersist fails with a clear error and creates no file:

  $ rm ocsidb
  $ dune exec -- ./test_no_db_file.exe
  init: Ocsipersist: no database file. Call Ocsipersist_settings.set_db_file before using Ocsipersist, or, with Ocsigen Server's configuration file, give <database file="..."/> to ocsipersist-sqlite-config.
  get: Ocsipersist: no database file. Call Ocsipersist_settings.set_db_file before using Ocsipersist, or, with Ocsigen Server's configuration file, give <database file="..."/> to ocsipersist-sqlite-config.
  $ test ! -e ocsidb
