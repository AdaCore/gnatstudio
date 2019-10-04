"""
Verify that GNAT Studio can set breakpoint on procedure.

"""
import GPS
from gps_utils.internal.utils import *


@run_test_driver
def test_driver():
    mode = "Mode:" + GPS.Preference("GPS6-Debugger-Debugger-Kind").get()

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = b.current_view()
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    debug = GPS.Debugger.get()

    b.current_view().goto(b.at(3, 12))
    select_editor_contextual("Debug/Set breakpoint on Main")
    yield wait_idle()

    gps_assert(len(debug.breakpoints), 1,
               "Wrong count of breakpoints in" + mode)
