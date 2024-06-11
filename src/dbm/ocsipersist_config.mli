(**

If you are using the DBM backend of Ocsipersist 
with Ocsigen Server with a configuration file,
install package [ocsipersist-dbm-config] and
use the following configuration tags.
All sub-tags of the [extension] tag are optional.

{@xml[
<extension findlib-package="ocsipersist-dbm-config">
  <delayloading val="false"/>
  <store dir="store"/>
  <ocsidbm name="ocsidbm"/>
</extension>
]}
*)
