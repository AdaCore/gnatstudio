"""
#171 Test that run command is enabled when the debuggee is terminated
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_tasks()

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    # Run the debugger
    debug = GPS.Debugger.get()
    debug.send("run")
    yield hook("debugger_process_terminated")
    yield wait_idle()

    # At this stage, the debuggee process has been terminated
    # Adding a breakpoint to ensure that the process is really restarted
    debug.break_at_location(GPS.File("main.adb"), 5)
    yield wait_DAP_server("setBreakpoints")
    yield wait_idle()

    # Re-run the debugger after it has been terminated
    debug.send("run")
    yield wait_DAP_server("stackTrace")
    yield wait_idle()

    # Check that we reach the previously set breakpoint
    yield wait_until_true(
        lambda: buf.current_view().cursor() == buf.at(5, 1),
        timeout=3000,
        error_msg=(
            "Breakpoint on main.adb:5 has not been reached when re-running the "
            "debugger"
        ),
    )
