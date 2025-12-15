v="$(gdb -v | head -n 1 | cut -c 14-16)"
if [ $v -ge 18 ]
then
  $GPS -Pdefault --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP --traceoff=DAP.CLIENTS.VARIABLES --traceoff=GPS.DAP.VARIABLES
fi
