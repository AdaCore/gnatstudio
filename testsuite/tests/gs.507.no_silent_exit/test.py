"""
Test that we have a message when process can't be started.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    yield wait_tasks()
    GPS.execute_action("Run Main Number 1")
    yield wait_tasks()

    gps_assert(
        "Could not locate executable" in GPS.Console().get_text(),
        True,
        "We should have a message warning that the executable could "
        + "not be found in the Messages view",
    )
