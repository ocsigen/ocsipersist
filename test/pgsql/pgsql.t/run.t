Test Functorial.length and Ref with a local ephemeral PostgreSQL instance.

Set up a temporary PostgreSQL cluster:

  $ PGBINDIR=$(pg_config --bindir)
  $ PGDATA=$(mktemp -d)
  $ $PGBINDIR/initdb -D "$PGDATA" --no-locale -E UTF8 -A trust > /dev/null
  $ $PGBINDIR/pg_ctl start -D "$PGDATA" -l "$PGDATA/pg.log" -o "-k $PGDATA -h '' -p 5444" -w > /dev/null
  $ export PGHOST="$PGDATA" PGPORT=5444 PGDATABASE=postgres

Run the test. A table and a persistent reference used before Ocsipersist.init
fail, and work after it:

  $ dune exec -- ./test.exe
  length before init: Ocsipersist db not initialised
  ref before init: Ocsipersist db not initialised
  length after 3 adds: 3
  fold count: 3
  fold concat: a=1;b=2;c=3;
  iter keys: a,b,c
  length after remove: 2
  ref after init: 1

Tear down:

  $ $PGBINDIR/pg_ctl stop -D "$PGDATA" -m fast > /dev/null
  $ rm -rf "$PGDATA"
