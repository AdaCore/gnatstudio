"""
Verify that GNAT Studio can set breakpoint on procedure.

"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)
    debug.start()
    yield hook("debugger_location_changed")

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = b.current_view()
    view.goto(b.at(14, 15))
    GPS.MDI.get("main.adb").raise_window()
    yield process_all_events()
    yield wait_idle()
    yield wait_until_true(
        lambda: GPS.Action("debug set line breakpoint").can_execute() == False
    )
    yield wait_idle()
    select_editor_contextual("Debug/Set breakpoint on P2")
    yield wait_DAP_server("setFunctionBreakpoints")

    gps_assert(len(debug.breakpoints), 1, "Wrong count of breakpoints")
