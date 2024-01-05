
mkdir obj
$GPS -Pprj --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP --traceoff=GPS.DAP.VARIABLES
rm -r obj
