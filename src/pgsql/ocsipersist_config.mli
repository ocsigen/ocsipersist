(**
If you are using the PostgreSQL backend of Ocsipersist 
with Ocsigen Server with a configuration file,
install package [ocsipersist-pgsql-config] and
use the following configuration tags.
All attributes of the [database] tag are optional.

One can either define a host to connect to:
{@xml[
<extension findlib-package="ocsipersist-pgsql-config">
  <database
     host="localhost"
     port="3000"
     user="ocsipersist"
     password="<secret>"
     size_conn_pool = "16"
  />
</extension>
]}

Or a UNIX domain socket:
{@xml[
<extension findlib-package="ocsipersist-pgsql-config">
  <database
    unix_domain_socket_dir = "pgsql_socket"
    user="ocsipersist"
    password="<secret>"
    size_conn_pool = "16"
  />
</extension>
]}

*)
