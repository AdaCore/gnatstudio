gprbuild -p -q
./obj/main_t819_019 &
TESTPID=`pidof -s main_t819_019`
export TESTPID
$GPS -Ptest.gpr --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
