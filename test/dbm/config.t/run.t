The configuration extension needs <store dir="..."/>: there is no default
directory for the database. The server refuses to start without it.

  $ mkdir -p log
  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe missing.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Ocsipersist: .*' out.log
  Ocsipersist: <store dir="..."/> is missing. It gives the directory of the database (Ocsigen Server's <datadir> is no longer used).

With it, the server starts and launches ocsidbm, which creates the
directory. The server is stopped through its command pipe.

  $ sed "s|OCSIDBM|$(command -v ocsidbm)|" database.conf.in >database.conf
  $ ./_build/default/test.exe database.conf >out.log 2>&1 &
  $ server=$!
  $ while [ ! -p local.cmd ] && kill -0 $server 2>/dev/null; do sleep 0.01; done
  $ echo shutdown >local.cmd; wait $server; echo "exit $?"
  exit 0
  $ grep -o "Initialization complete" out.log
  Initialization complete
  $ test -d store

Without a log directory, the server logs to its standard error, and ocsidbm
writes its errors there too. Here ocsidbm cannot create its socket, since a
file of that name is in the way, so the server fails.

  $ mkdir busy && touch busy/socket
  $ sed "s|OCSIDBM|$(command -v ocsidbm)|" nolog.conf.in >nolog.conf
  $ ./_build/default/test.exe nolog.conf >out.log 2>&1; echo "exit $?"
  exit 9
  $ grep -o -m 1 'Ocsidbm - Please make sure that the directory busy exists' out.log
  Ocsidbm - Please make sure that the directory busy exists
