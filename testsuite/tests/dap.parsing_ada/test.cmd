
### Description
## Parse variables in Ada code

v="$(gdb -v | head -n 1 | cut -c 14-16)"
if [ $v -ge 15 ]
then
  gprbuild -q -Pgvd_ada
  $GPS --load=python:test.py --debug=obj/parse --traceon=GPS.DEBUGGING.DAP_MODULE --traceon=MODULE.Debugger_DAP
fi
