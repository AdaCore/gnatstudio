"""
Tests that we have warning in the debugger console when executable
does not have debug information.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    yield wait_tasks()
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_for_mdi_child("Debugger Console")
    yield wait_idle()
    gps_assert(
        "proper debug" in GPS.Debugger.get().get_console().get_text(),
        True,
        "No warning",
    )
