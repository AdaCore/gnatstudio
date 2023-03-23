$GPS -Pdefault --load=test.py --traceoff=GPS.DEBUGGING.Gdb_MI --traceon=MODULE.Debugger_Gdb
rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.db
rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.json
$GPS -Pdefault --load=test.py --traceoff=GPS.DEBUGGING.Gdb_MI --traceon=MODULE.Debugger_Gdb_MI

v="$(gdb -v | head -n 1 | cut -c 14-16)"
if [ $v -gt 12 ]
then
  rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.db
  rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.json
  $GPS -Pdefault --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
fi
