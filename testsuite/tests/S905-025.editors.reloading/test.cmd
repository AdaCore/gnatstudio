rm -f hello.txt
echo "no text" > hello.txt
$GPS --load=python:test.py --traceon=TESTSUITE_TIMESTAMP_CHECKS
