"""
Test that breakpoint can be set while executable is running
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # Start the debugger
    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)
    debug.send("run")

    # wait until executable is started
    yield wait_for_mdi_child("Debugger Execution main")

    # set new breakpoint
    yield wait_idle()
    debug.break_at_location(GPS.File("main.adb"), 9)

    # wait while debugger is stopped on the new BP
    yield hook("debugger_location_changed")
