gprbuild -P ./install/fake_executables.gpr -p -q
PATH=`pwd`/install/bin:$PATH
export PATH
$GPS --load=python:test.py
