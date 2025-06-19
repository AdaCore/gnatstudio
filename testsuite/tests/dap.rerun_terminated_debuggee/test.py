"""
#171 Test that run command is enabled when the debuggee is terminated
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    # Run the debugger
    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_until_not_busy(debug)
    yield wait_idle()
    gps_assert(debug.is_busy(), False, "debugger should not be busy")

    # At this stage, the debuggee process has been terminated
    # Adding a breakpoint to ensure that the process is really restarted
    debug.break_at_location(GPS.File("main.adb"), 5)
    yield wait_until_not_busy(debug)
    yield wait_idle()

    debug.send("run")
    yield wait_DAP_server("stackTrace")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    # check that we set the cursor on the breakpoint line
    # which means that we stopped on the breakpoint
    gps_assert(buf.current_view().cursor(), buf.at(5, 1))
