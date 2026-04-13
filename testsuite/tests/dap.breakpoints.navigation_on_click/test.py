"""
Test that double-clicking on a breakpoint in the Breakpoints view
navigates to the correct source location for source location (On_Line)
and subprogram (On_Subprogram) breakpoints.
"""

import GPS
from gs_utils.internal.utils import (
    gps_assert,
    Breakpoints_View,
    run_test_driver,
    wait_tasks,
    wait_for_mdi_child,
    wait_idle,
    wait_until_not_busy,
    wait_DAP_server,
    click_in_tree,
    hook,
)
import pygps

# Line where we'll set the source location breakpoint
SLOC_LINE = 16

# Line where the Hello subprogram starts
HELLO_LINE = 5


def verify_navigation(expected_line, description):
    """
    Verify that main.adb is opened and the cursor is at the expected line.
    Closes the buffer after verification.
    """
    editor = GPS.MDI.get("main.adb")
    gps_assert(
        editor is not None,
        True,
        f"main.adb should be opened after double-clicking ({description})",
    )

    buf = GPS.EditorBuffer.get(GPS.File("main.adb"), open=False)
    loc = buf.get_cursors()[0].location()
    gps_assert(
        loc.line(),
        expected_line,
        f"Cursor should be at line {expected_line} after double-click ({description})",
    )
    buf.close()


@run_test_driver
def test_driver():
    yield wait_tasks()

    # Open main.adb and set a source location breakpoint
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_for_mdi_child("main.adb")

    buf.current_view().goto(buf.at(SLOC_LINE, 1))
    yield wait_idle()
    GPS.execute_action("debug set line breakpoint")

    # Close the editor so we can verify it opens on double-click
    buf.close()
    yield wait_idle()

    # Open the Breakpoints view
    bp_view = Breakpoints_View()
    yield wait_tasks()

    # Test 1: Without debugger - Double-click on source location breakpoint
    GPS.Console().write(
        "Test 1: Without debugger - Double-click on source location breakpoint\n"
    )
    click_in_tree(bp_view.list, path="0", events=pygps.double_click_events, column=1)
    verify_navigation(SLOC_LINE, "source location breakpoint without running debugger")
    yield wait_idle()

    # Start the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)
    debug.send("start")
    yield wait_DAP_server("stackTrace")

    # Test 2: With running debugger - Double-click on source location breakpoint
    click_in_tree(bp_view.list, path="0", events=pygps.double_click_events, column=1)
    yield wait_idle()
    verify_navigation(SLOC_LINE, "source location breakpoint with running debugger")
