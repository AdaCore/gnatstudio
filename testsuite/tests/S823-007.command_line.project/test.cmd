# The -P is explicitely missing: this is the purpose of the test
$GPS foobar.gpr --load=python:test_alone.py

$GPS foobar.gpr main.adb --load=python:test_together.py

$GPS foobar.gpr foo.gpr --load=python:test_together.py
