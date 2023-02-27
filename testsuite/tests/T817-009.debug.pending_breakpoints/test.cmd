$GPS -Pmain --load=test.py --traceoff=GPS.DEBUGGING.Gdb_MI --traceon=MODULE.Debugger_Gdb
$GPS -Pmain --load=test.py --traceoff=GPS.DEBUGGING.Gdb_MI
if [ "$DAP_GDB" != "" ]; then
  $GPS -Pmain --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
fi

