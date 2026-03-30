v="$(gdb -v | head -n 1 | cut -c 14-16)"
if [ $v -ge 15 ]
then
  gprbuild -Pdefault -p -q
  $GPS -Pdefault --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP --traceoff=GPS.DAP.REQUESTS_BREAKPOINTS --traceoff=GPS.DAP.REQUESTS_FUNCTION_BREAKPOINTS --traceoff=GPS.DAP.REQUESTS_INSTRUCTION_BREAKPOINTS --traceoff=GPS.DAP.REQUESTS_EXCEPTION_BREAKPOINTS
fi
