The configuration extension needs <database file="..."/>: there is no default
database file. The server refuses to start without it.

  $ mkdir -p log
  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe missing.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Ocsipersist: .*' out.log
  Ocsipersist: <database file="..."/> is missing. It gives the database file (Ocsigen Server's <datadir> is no longer used).

With it, the server starts. It is stopped through its command pipe.

  $ ./_build/default/test.exe database.conf >out.log 2>&1 &
  $ while [ ! -p local.cmd ]; do sleep 0.01; done
  $ echo shutdown >local.cmd; wait; echo "exit $?"
  exit 0
