"""
Test that the Breakpoints view gets correctly updated
when having several breakpoints across different files,
some being disabled too.
"""

import GPS
from gs_utils.internal.utils import *
from workflows import run_as_workflow


@run_test_driver
def test_driver():
    yield wait_tasks()

    # Create 2 SLOC breakpoints before launching the debugger
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_for_mdi_child("main.adb")

    # Create breakpoints
    for i in range(11, 13):
        buf.current_view().goto(buf.at(i, 1))
        GPS.process_all_events()
        yield wait_idle()
        yield wait_until_true(
            lambda: GPS.Action("debug set line breakpoint").can_execute() == False
        )
        yield wait_idle()
        GPS.execute_action("debug set line breakpoint")
        yield wait_tasks()

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

    # Check that we still have 2 breakpoints in the view
    breakpoints = debug.breakpoints
    gps_assert(len(breakpoints), 2, "Should have 2 breakpoints")
