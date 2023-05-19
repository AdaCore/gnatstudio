"""
Verify that GNAT Studio can set breakpoint on procedure.

"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    mode = "Mode:" + GPS.Preference("GPS6-Debugger-Debugger-Kind").get()

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    debug = GPS.Debugger.get()

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = b.current_view()
    view.goto(b.at(3, 12))
    yield wait_idle()

    select_editor_contextual("Debug/Set breakpoint on Main")
    if mode == "Mode:Dap":
        yield wait_DAP_server("setFunctionBreakpoints")

    gps_assert(len(debug.breakpoints), 1,
               "Wrong count of breakpoints in " + mode)
