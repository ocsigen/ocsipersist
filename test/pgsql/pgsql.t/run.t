Test Functorial.length and Ref with a local ephemeral PostgreSQL instance.

The test of modify_opt is shared by the backends: copy it here first, since
the dune file of this directory refers to it:

  $ cp ../../common/modify_opt_race.ml .

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

modify_opt creates, removes, and leaves the value unchanged when f raises.
Two processes of four threads each increment a counter with modify_opt at
the same time, and no increment is lost:

  $ dune exec -- ./test_modify_opt.exe
  created: new
  created: absent
  modify_opt: f failed
  kept: v
  expected counter: 400
  counter: 400

Tear down:

  $ $PGBINDIR/pg_ctl stop -D "$PGDATA" -m fast > /dev/null
  $ rm -rf "$PGDATA"
