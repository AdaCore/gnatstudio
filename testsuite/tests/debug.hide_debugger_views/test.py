"""
Verify that debugger views are hidden when closing the debugger session.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    expected = len(GPS.MDI.children())

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    debug = GPS.Debugger.get()

    # Open the assembly and memory view
    GPS.execute_action("open assembly view")
    yield wait_for_mdi_child("Assembly")
    gps_assert(GPS.MDI.get("Assembly") is not None,
               True,
               "The Assembly view should be opened")
    GPS.execute_action("examine memory")
    yield wait_for_mdi_child("Memory")
    gps_assert(GPS.MDI.get("Memory") is not None,
               True,
               "The Memory view should be opened")

    # Closing the debugger
    debug.send("q")
    gps_assert(len(GPS.MDI.children()),
               expected,
               "The debugger views should be closed/hidden")
