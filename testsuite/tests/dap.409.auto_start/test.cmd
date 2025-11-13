v="$(gdb -v | head -n 1 | cut -c 14-16)"
if [ $v -ge 15 ]
then
    mkdir obj
    gprbuild -Ptest -p -q
    $GPS -Ptest --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
    rm -r obj
fi
