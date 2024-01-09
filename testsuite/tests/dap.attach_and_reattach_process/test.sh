# The DAP attach request works properly starting from GDB 15
v="$(gdb -v | head -n 1 | cut -c 14-16)"
if [ $v -gt 15 ]
then
    gprbuild -p -q
    ./obj/main_t819_019&
    TESTPID=`pidof -s main_t819_019`
    export TESTPID
    $GPS -Ptest.gpr --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
    kill -9 $TESTPID
fi
