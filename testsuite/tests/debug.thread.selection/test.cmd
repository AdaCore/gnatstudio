# Only test the MI thread view, the columns are too different (6 for MI, 1 for CLI)
$GPS -Pdefault --load=test.py --traceoff=GPS.DEBUGGING.Gdb_MI --traceon=MODULE.Debugger_Gdb_MI

