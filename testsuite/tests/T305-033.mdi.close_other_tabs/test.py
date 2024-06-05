"""
This test checks the good behavior of the "Close all windows except current"
action.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    initial_nb_children = len(GPS.MDI.children())
    GPS.EditorBuffer.get(GPS.File("a.adb"))
    GPS.EditorBuffer.get(GPS.File("b.adb"))
    GPS.execute_action("open Welcome")
    yield wait_for_mdi_child("Welcome")

    gps_assert(
        len(GPS.MDI.children()),
        initial_nb_children + 3,
        "other views have not been properly opened",
    )

    GPS.execute_action("Close all windows except current")

    gps_assert(
        GPS.MDI.current().name(short=True),
        "Welcome",
        "The Welcome view should still be opened and focused",
    )
    gps_assert(
        len(GPS.MDI.children()),
        initial_nb_children + 1,
        "The other notebook tabs have not been closed properly",
    )
