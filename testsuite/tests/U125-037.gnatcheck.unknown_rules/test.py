"""
This test checks that we correctly detect rules that are unknown
by gnatcheck
"""
import GPS
from gs_utils.internal.utils import *
import os.path


@run_test_driver
def test_driver():
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("gnatcheck root project")
    yield wait_tasks(other_than=known_tasks)

    location = GPS.Locations.list_locations(
        "Coding Standard Rules", os.path.join(GPS.pwd(), "coding_standard.txt")
    )[0]

    gps_assert(
        str(location), "coding_standard.txt:1:1", "Unknown rule has not been detected"
    )
