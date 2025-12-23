"""
Verify that closing Debugger Execution console does not
cause exceptions.
"""

from gs_utils.internal.utils import *
from workflows import promises


@run_test_driver
def driver():
    yield wait_tasks()
    GPS.execute_action("Build & Debug Number 1")
    yield wait_for_mdi_child("Debugger Console")

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))

    p = promises.DebuggerWrapper(GPS.File("main"))
    debug = p.get()
    yield wait_until_not_busy(debug)

    yield p.send_promise("break main.adb:5")
    yield wait_DAP_server("setBreakpoints")
    yield wait_idle()

    yield p.send_promise("run")
    yield hook("debugger_location_changed")
    yield wait_idle()

    GPS.MDI.get("Debugger Execution main" + dot_exe).close()

    # Check that we closed debugger session properly
    yield wait_until_true(lambda: GPS.MDI.current_perspective() != "Debug")
    gps_assert(GPS.MDI.current_perspective() != "Debug", True, "Wrong perspective")
