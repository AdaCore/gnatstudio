"""
Tests that we can terminate debuger when initialized with no main file
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    yield wait_tasks()
    dialog = None
    GPS.execute_action("/Debug/Initialize/no main file")
    yield wait_for_mdi_child("Debugger Console")

    GPS.execute_action("terminate all debuggers")
    yield wait_until_true(lambda: GPS.MDI.current_perspective() != "Debug")

    gps_assert(GPS.MDI.current_perspective() != "Debug", True, "Wrong perspective")
