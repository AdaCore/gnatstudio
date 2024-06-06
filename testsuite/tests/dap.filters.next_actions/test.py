"""
This test checks that debug next/step actions
are available only when the debuggee has been started and stopped.
"""

import GPS
from gs_utils.internal.utils import *

DEBUG_ACTIONS = ["debug next", "debug nexti", "debug step", "debug stepi"]


@run_test_driver
def test_driver():
    # Open main.adb and set a breakpoint line 5
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(5, 1))
    yield hook("location_changed", debounced=True)
    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks(other_than=known_tasks)

    # Start a debugging session
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    # Check that the debug next/step actions are not available before starting
    # the debuggee
    is_available = all([GPS.Action(a).can_execute() for a in DEBUG_ACTIONS])
    gps_assert(
        is_available,
        False,
        "'debug next' and 'debug nexti' should not be available before starting the debuggee",
    )

    # Run the debugger
    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_DAP_server("stackTrace")

    # Check that the debug next/step actions are now available
    is_available = all([GPS.Action(a).can_execute() for a in DEBUG_ACTIONS])
    gps_assert(
        is_available,
        True,
        "'debug next' and 'debug nexti' should be available once the debuggee is started and stopped",
    )
