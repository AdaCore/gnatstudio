mkdir -p $GNATSTUDIO_HOME/.gps/
cp properties.json $GNATSTUDIO_HOME/.gps/

$GPS --load=python:test.py
