"""
Test that the GS UI is correctly refreshed when sending
commands to the GDB DAP server directly via the console: we
should correctly react to DAP 'breakpoint' events.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    view = Breakpoints_View()

    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)

    debug.send("start")
    yield wait_DAP_server("stackTrace")

    # Create two SLOC breakpoints and one on a subprogram
    yield debug.send("b main.adb:9")
    yield wait_until_not_busy(debug)
    yield debug.send("b main.adb:11")
    yield wait_until_not_busy(debug)
    yield debug.send("b print")
    yield wait_until_not_busy(debug)

    breakpoints = debug.breakpoints
    gps_assert(len(breakpoints), 3, "Should have 3 breakpoints")

    # This will delete the breakpoint at the current location, so
    # the first breakpoint we hit, a line 9.
    debug.send("clear")
    yield hook("debugger_breakpoint_deleted")

    debug.send("clear print")
    yield hook("debugger_breakpoint_deleted")

    # Verify that we have correctly removed the corresponding breakpoints
    # in the Breakpoints' view and the editor's side column.
    breakpoints = debug.breakpoints
    bp_messages = GPS.Message.list("breakpoints", GPS.File("main.adb"))
    gps_assert(len(bp_messages), 1, "Should have 1 breakpoint in the editor")
    gps_assert(len(breakpoints), 1, "Should have 1 breakpoint in the Breakpoints view")
    gps_assert(
        (breakpoints[0].file, breakpoints[0].line),
        (GPS.File("main.adb"), 11),
        "Remaining breakpoint should be at main.adb:11",
    )
