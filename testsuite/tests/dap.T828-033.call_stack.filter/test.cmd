v="$(gdb -v | head -n 1 | cut -c 14-16)"
if [ $v -gt 15 ]
then
  $GPS -Ptest --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
fi
