"""
Test that added Python API methods works as expected.
"""

import GPS
from gs_utils.internal.utils import *
from workflows import run_as_workflow
from workflows import promises


@run_test_driver
def test_driver():
    yield wait_tasks()

    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_for_mdi_child("main.adb")

    # Create breakpoint
    buf.current_view().goto(buf.at(7, 1))
    GPS.process_all_events()
    yield wait_until_true(
        lambda: GPS.Action("debug set line breakpoint").can_execute() == False
    )
    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks()

    # Start the debugger and run it
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)

    debug.send("run")
    yield hook("debugger_location_changed")

    # Check that we still have 2 breakpoints in the view
    Num = debug.get_num()
    gps_assert(Num, 1, "NUM should be 1")
    Lst = GPS.Debugger.list()
    gps_assert(len(Lst), 1, "Should have 1 debugger")
    gps_assert(Lst[0].get_num(), Num, "Numbers should be equal")
    gps_assert(debug.is_connected_remotely, False, "Should not be connected remotely")
    gps_assert(debug.remote_protocol, "", "Remote protocol should be empty")
    gps_assert(debug.remote_target, "", "Remote target should be empty")
    gps_assert(debug.command(), "", "Command should be empty")
    gps_assert(debug.is_break_command(), False, "is_break_command should be False")
    gps_assert(debug.is_context_command(), False, "is_context_command should be False")
    gps_assert(debug.is_exec_command(), False, "is_exec_command should be False")
    gps_assert(debug.is_exec_command(), False, "is_exec_command should be False")

    p = promises.DebuggerWrapper(GPS.File("main.adb"))
    Val = yield p.value_of("A")
    gps_assert("= 3" in Val.data, True, "A should be 3, have '" + Val.data + "'")

    debug.set_variable("A", "4")
    yield wait_until_not_busy(debug)
    Val = yield p.value_of("A")
    gps_assert("= 4" in Val.data, True, "A should be 4, have '" + Val.data + "'")
