
# lldb doesn't support ada
#if [ "$OS" == "Windows_NT" ]; then
  list="Gdb Gdb_MI"
#else
#  if which lldb >/dev/null 2>&1; then
#    list="Gdb Gdb_MI LLDB"
#  else
#    list="Gdb Gdb_MI"
#  fi
#fi

mkdir obj
for gdb in $list; do
  rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.db
  rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.json
  $GPS -Pprj --load=test.py --traceoff=GPS.DEBUGGING.Gdb_MI --traceon=MODULE.Debugger_$gdb
done
rm -r obj
