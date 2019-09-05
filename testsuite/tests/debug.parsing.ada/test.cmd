
### Description
## Parse variables in Ada code

gprbuild -q -Pgvd_ada

list="Gdb Gdb_MI"
#list="Gdb Gdb_Pretty_Printer Gdb_MI"

for gdb in $list; do
  rm -rf $GPS_HOME/.gps/properties.db
  rm -rf $GPS_HOME/.gps/properties.json
  $GPS --traceon=MODULE.Debugger_$gdb --load=python:test.py --debug=obj/parse
done
