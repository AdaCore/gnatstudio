v="$(gdb -v | head -n 1 | cut -c 14-16)"
if [ $v -gt 12 ]
then
  $GPS -Pdefault --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP --traceoff=GPS.DAP.VARIABLES
fi
