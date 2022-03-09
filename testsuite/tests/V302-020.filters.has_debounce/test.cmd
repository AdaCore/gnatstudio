cp -f histories.xml $GNATSTUDIO_HOME/.gnatstudio/
$GPS --load=python:test.py -P default.gpr
