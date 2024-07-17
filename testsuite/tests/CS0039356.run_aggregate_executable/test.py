"""Tests that we can execute Build/Project/Build & Run
for the aggregate project with Exec_Dir
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    yield wait_tasks()
    GPS.execute_action("Build & Run Number 1")
    GPS.execute_action("Build & Run Number 2")
    yield wait_idle()
    gps_assert(
        "Error while trying to execute" in GPS.Console("Messages").get_text(), False
    )
