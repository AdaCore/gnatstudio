gprbuild -p -q
./obj/main_sa17_019 &
TESTPID=`pidof -s main_sa17_019`
export TESTPID
$GPS -Ptest.gpr --load=test.py --traceoff=GPS.DEBUGGING.Gdb_MI --traceon=MODULE.Debugger_Gdb_MI
kill $TESTPID
