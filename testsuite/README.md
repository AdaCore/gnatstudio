## GPS testsuite

### Writing tests

Tests are in the directory `tests`, with one subdirectory per test.

Each test should contain:
- a `test.py` file, which is loaded by GPS.
- a `test.yaml` file which contains metadata needed to run the test.

As an example, look at the following tests:
- `tests/minimal` - this test contains the minimal test framework:
copy this to start a new test
- `tests/Z999-999.xfail` - this test contains a test which is expected to fail.

To test for functionality, use the GPS scripting API, and in particular make use
of `gps_utils.internal.utils.gps_assert`:

    gps_assert(something_observed, something_expected, "message to emit")

### Running tests

To run the entire testsuite:

    ./run.sh

To run one individual test pass its directory as parameter:

    ./run.sh tests/minimal/

The complete results are in the out/ directory.
