if [ "`type gnatfuzz 2>/dev/null`" = "" ]; then
  # Mark test as DEAD if gnatfuzz is not found
  exit 99
fi

export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1
export AFL_SKIP_CPUFREQ=1

$GNATSTUDIO --load=python:test.py -Pp.gpr
