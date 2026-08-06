# Make available the fake alr
export PATH=`pwd`:$PATH
$GPS --load=python:test.py -P plain/plain.gpr
