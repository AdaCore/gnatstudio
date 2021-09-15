# Make available the fake cleartool (linux) and cleartool.exe (windows)
export PATH=`pwd`:$PATH
$GPS --load=python:test.py
