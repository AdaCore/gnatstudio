"""
This test checks that we correctly invoke gnatcheck on non-native
projects.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("gnatcheck file")
    yield wait_tasks(other_than=known_tasks)

    # If GNATcheck succeed to run, we should have a message in the
    # Locations view complaining about the comments style
    msgs = GPS.Message.list(file=GPS.File("main.adb"),
                            category="Coding Standard violations")
    gps_assert(len(msgs), 1, "No gnatcheck messages in the Locations")
