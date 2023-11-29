
"""
This test checks that the good behavior of pending breakpoints
in GNAT Studio.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.Preference("Debugger-Pending-Breakpoints").set(True)

    # Set a breakpoint in the Ada library
    buf = GPS.EditorBuffer.get(GPS.File("p.adb"))
    buf.current_view().goto(buf.at(8, 1))
    yield wait_idle()
    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks()

    # Build and debug the C main: the Ada library is not loaded
    # at this stage so the breakpoint should be marked as pending
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    # Run the debugger and verify that we reach the breakpoint
    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)
    debug.send("run")
    yield wait_until_not_busy(debug)
    
    gps_assert(debug.current_file, GPS.File("p.adb"),
               "The pending breakpoint has not been reached")
    gps_assert(debug.current_line, 8,
               "The pending breakpoint has not been reached")
               
