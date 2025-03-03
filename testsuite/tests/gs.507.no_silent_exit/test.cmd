cp -f targets.xml $GNATSTUDIO_HOME/.gnatstudio/
$GPS -Ptest --load=python:test.py
