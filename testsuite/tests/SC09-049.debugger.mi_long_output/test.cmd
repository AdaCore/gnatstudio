$GPS -Ptest.gpr --load=test.py --traceon=MODULE.Debugger_Gdb_MI

if [ "`type qgenc 2>/dev/null`" = "" ]; then
  # Do not run test with qgen
  v="$(gdb -v | head -n 1 | cut -c 14-16)"
  if [ $v -gt 12 ]
  then
    $GPS -Ptest.gpr --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
  fi
fi
