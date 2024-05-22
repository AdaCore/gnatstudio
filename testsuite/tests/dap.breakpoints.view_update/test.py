"""
Test that the Breakpoints view gets correctly updated
when having several breakpoints across different files,
some being disabled too.
"""

import GPS
from gs_utils.internal.utils import *
from workflows import run_as_workflow


@run_as_workflow
def create_breakpoint_at_location(file, line):
    buf = GPS.EditorBuffer.get(GPS.File(file))
    buf.current_view().goto(buf.at(line, 1))
    yield wait_idle()
    GPS.execute_action("debug set line breakpoint")

@run_test_driver
def test_driver():
    # Create 3 SLOC breakpoints before launching the debugger
    yield create_breakpoint_at_location("main.adb", 11)
    yield create_breakpoint_at_location("main.adb", 12)
    yield create_breakpoint_at_location("test.ads", 5)

    # Open the Breakpoints view
    view = Breakpoints_View()

    # Disable the first breakpoint
    yield view.select(0)
    GPS.execute_action("debug disable breakpoints")
    yield wait_idle()

    # Start the debugger and run it
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)
    debug.send("run")
    yield wait_DAP_server("stackTrace")

    # Check that we still have 3 breakpoints in the view
    breakpoints = debug.breakpoints
    gps_assert(len(breakpoints), 3, "Should have 3 breakpoints")
