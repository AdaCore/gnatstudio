"""
This test checks that we do not ask for a rules file if the gnatcheck
rules are directly defined in the .gpr file itself.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("gnatcheck root project")
    yield wait_tasks(other_than=known_tasks)

    location = GPS.Locations.list_locations(
        "Coding Standard violations",
        os.path.join(GPS.pwd(), 'main.adb'))[0]

    gps_assert(str(location), "main.adb:3:7",
               "gnatcheck has not been launched without a rules file")
