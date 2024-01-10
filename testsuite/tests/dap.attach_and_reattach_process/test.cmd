# The DAP attach request works properly starting from GDB 15
v="$(gdb -v | head -n 1 | cut -c 14-16)"
if [ $v -ge 15 ]
then
    $GPS -Ptest.gpr --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
fi