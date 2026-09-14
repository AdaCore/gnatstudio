if [ "`type gnatfuzz 2>/dev/null`" = "" ]; then
  # Mark test as DEAD if gnatfuzz is not found
  exit 99
fi

export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1
export AFL_SKIP_CPUFREQ=1

# gnatfuzz analyze to find the fuzzable subprograms
gnatfuzz analyze -Pp.gpr > /dev/null

# gnatfuzz generate to generate a fuzz harness
gnatfuzz generate -Pp.gpr --analysis obj/gnatfuzz/analyze.json --subprogram-id 1 -o harness > /dev/null

# Run the test. The generated harness directory is "fuzz"; older
# gnatfuzz used "fuzz_testing". Accept either, as the plugin does.
HARNESS_DIR=harness/fuzz
if [ ! -d "$HARNESS_DIR" ] && [ -d harness/fuzz_testing ]; then
  HARNESS_DIR=harness/fuzz_testing
fi

$GNATSTUDIO --load=python:test.py -P $HARNESS_DIR/fuzz_test.gpr