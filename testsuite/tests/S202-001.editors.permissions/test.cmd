# This test is not runnable as root
[ "$USER" == "root" ] && exit 99

$GNATSTUDIO --load=python:test.py
