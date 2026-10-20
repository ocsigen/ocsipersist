(**
If you are using the SQLite backend of Ocsipersist with Ocsigen Server
with a configuration file, install package [ocsipersist-sqlite-config] and
use the following configuration tags.
[<database>] is required: it gives the database file, in an existing
directory.

{@xml[
<extension findlib-package="ocsipersist-sqlite-config">
  <database file="data/ocsidb"/>
</extension>
]}
*)
