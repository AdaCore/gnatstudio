"""
Test that DAP exception breakpoints with specific exception filters
work correctly in GNAT Studio.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def on_gps_started():
    # Create an exception breakpoint for Constraint_Error
    breakpoints_view = Breakpoints_View()
    ed = breakpoints_view.create()
    yield ed.open_and_yield()
    yield ed.create_exception_breakpoint("Constraint_Error")

    # Start debugging
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # Now run the program to hit the exception breakpoint.
    # The Constraint_Error exception should be raised at
    # line 9: check that we hit this line.
    debug = GPS.Debugger.get()
    debug.send("run")
    yield hook("debugger_location_changed")
    gps_assert(
        debug.current_line,
        9,
        "The Constraint_Error exception breakpoint was not hit as expected",
    )

    # Continue the execution: the exception should be caught
    # and the program should terminate normally.
    # Even if another exception is raised (Program_Error at line 11),
    # we should not stop there since the breakpoint filter is
    # set to Constraint_Error only.
    debug.send("continue")
    yield hook("debugger_process_terminated")
    gps_assert(debug.current_line, 0, "The program did not terminate as expected")
