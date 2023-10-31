if [ "`type qgenc 2>/dev/null`" = "" ]; then
  # Do not run test with qgen
  v="$(gdb -v | head -n 1 | cut -c 14-16)"
  if [ $v -gt 12 ]
  then
    rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.db
    rm -rf $GNATSTUDIO_HOME/.gnatstudio/properties.json
    $GPS -Pdefault --load=test.py --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
  fi
fi
