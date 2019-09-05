mkdir -p $GNATSTUDIO_HOME/.gnatstudio/
cp properties.json $GNATSTUDIO_HOME/.gnatstudio/

$GPS --load=python:test.py
