
### Description
## Parse variables in C code

gprbuild -q -Pgvd_c

list="Gdb Gdb_MI"
#list="Gdb Gdb_Pretty_Printer Gdb_MI"

for gdb in $list; do
  rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.db
  rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.json
  $GPS --traceon=MODULE.Debugger_$gdb --load=python:test.py --debug=obj/parse_c
done
