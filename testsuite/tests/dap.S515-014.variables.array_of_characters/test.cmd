v="$(gdb -v | head -n 1 | cut -c 14-16)"
if [ $v -ge 15 ]
then
    mkdir obj
    $GPS -Pprj --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP --traceoff=GPS.DAP.VARIABLES --traceoff=DAP.CLIENTS.VARIABLES --traceoff=DAP.VARIABLES_REQUEST
    rm -r obj
fi
