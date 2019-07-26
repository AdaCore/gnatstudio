rm -f not_src
ln -s src not_src
$GPS --load=python:test.py
rm -f not_src
