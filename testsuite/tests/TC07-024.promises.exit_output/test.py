
"""
This test checks that a failing promises will output the error and status.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    # The dir doesn't contain a .git => wait for git status to be run
    yield wait_tasks()
    output = GPS.Console().get_text()
    # Verify the error and status
    gps_assert("128" in output, True, "Missing status")
    gps_assert("not a git repository" in output, True, "Missing error")
