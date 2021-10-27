if [ "`type codepeer 2>/dev/null`" = "" ]; then
  # Mark test as DEAD if codepeer is not found
  exit 99 
fi

$GPS -Ptest --load=python:test.py
rm -rf codepeer
