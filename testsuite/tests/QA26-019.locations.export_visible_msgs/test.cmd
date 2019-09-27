rm -rf obj
mkdir -p obj
$GPS --load=python:test1.py
$GPS --load=python:test2.py
diff -u messages.expected.txt messages.txt
