if [ "`type gnatfuzz 2>/dev/null`" = "" ]; then
  # Mark test as DEAD if gnatfuzz is not found
  exit 99
fi

$GNATSTUDIO --load=python:test.py
