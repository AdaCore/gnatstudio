gprbuild -q -Ptest
$GPS --load=python:test.py -Ptest --traceon=MODULE.Debugger_Gdb
$GPS --load=python:test.py -Ptest --traceon=MODULE.Debugger_Gdb_MI
