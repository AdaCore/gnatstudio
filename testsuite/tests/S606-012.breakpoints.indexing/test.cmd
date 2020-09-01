# This test requires the default perspective
rm $GNATSTUDIO_HOME/.gnatstudio/perspectives*

$GNATSTUDIO --load=python:test.py
