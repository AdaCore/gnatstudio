$GPS -Pdefault --load=test.py --traceoff=GPS.DEBUGGING.Gdb_MI --traceon=MODULE.Debugger_Gdb
$GPS -Pdefault --load=test.py --traceoff=GPS.DEBUGGING.Gdb_MI --traceon=MODULE.Debugger_Gdb_MI

# not implemented, no such request in the protocol
#if [ "`type qgenc 2>/dev/null`" = "" ]; then
  # Do not run test with qgen
#  v="$(gdb -v | head -n 1 | cut -c 14-16)"
#  if [ $v -gt 12 ]
#  then
#    $GPS -Pdefault --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
#  fi
#fi

