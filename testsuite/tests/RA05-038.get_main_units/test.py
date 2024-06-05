"""
This test verifies GPS.Project.get_main_units() method works well
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        GPS.Project.get_main_units(), ["test.adb", "main.adb"], "Incorrect main units"
    )
