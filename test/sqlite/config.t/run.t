The configuration extension needs <database file="..."/>: there is no default
database file. The server refuses to start without it.

  $ mkdir -p log
  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe missing.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Ocsipersist: .*' out.log
  Ocsipersist: <database file="..."/> is missing. It gives the database file (Ocsigen Server's <datadir> is no longer used).

With it, the server starts and opens the database file. It is stopped through
its command pipe.

  $ ./_build/default/test.exe database.conf >out.log 2>&1 &
  $ server=$!
  $ while [ ! -p local.cmd ] && kill -0 $server 2>/dev/null; do sleep 0.01; done
  $ echo shutdown >local.cmd; wait $server; echo "exit $?"
  exit 0
  $ test -f ocsidb
