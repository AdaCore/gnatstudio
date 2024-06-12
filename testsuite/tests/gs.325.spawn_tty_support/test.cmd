# The simple presence of $ADA_DEBUG_FILE influences this test:
# work around this by having GNAT Studio generate a text file.
unset ADA_DEBUG_FILE
$GNATSTUDIO --load=python:test.py
cat console.txt | grep stdout