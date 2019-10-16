$GPS -Pdefault --load=test.py --traceoff=GPS.DEBUGGING.Gdb_MI --traceon=MODULE.Debugger_Gdb
rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.db
rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.json
#$GPS -Pdefault --load=test.py --traceon=MODULE.Debugger_Gdb_MI

