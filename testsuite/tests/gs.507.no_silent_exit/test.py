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
        "process exited with status" in GPS.Console("Run: foo" + dot_exe).get_text(),
        True,
        "Missing message in the Messages view",
    )