gcc fake_gnatlist.c -o fake_gnatlist
export PATH=.:$PATH
$GNATSTUDIO --load=python:test.py
cat output.txt
