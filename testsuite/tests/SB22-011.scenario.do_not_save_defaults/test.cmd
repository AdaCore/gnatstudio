$GPS --load=python:test.py

# verify that the value of the variable hasn't been recorded in the properties
grep "my_variable" $GNATSTUDIO_HOME/.gnatstudio/properties.json | tr '\n' '\0'
