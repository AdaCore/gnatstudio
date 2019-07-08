mkdir -p $GPS_HOME/.gps/
cp properties.json $GPS_HOME/.gps/

$GPS --load=python:test.py
