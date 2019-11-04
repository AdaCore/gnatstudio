
### Description
## Parse variables in CPP code

if [ "$CPP_TESTING" = "false" ]; then
   # do not test cpp
   exit 99
fi

gprbuild -q -Pgvd_cpp

list="Gdb Gdb_MI"
#list="Gdb Gdb_Pretty_Printer Gdb_MI"

for gdb in $list; do
  rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.db
  rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.json
  $GPS --traceon=MODULE.Debugger_$gdb --load=python:test.py --debug=obj/parse_cpp
done
