"""
This test checks that the DAP server does not hit breakpoints
that have been disabled once the debugger is startted.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    yield wait_tasks()
    # Open main.adb and set a breakpoint line 6
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(6, 1))
    yield hook("location_changed", debounced=True)
    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks(other_than=known_tasks)

    # Open test.adb and set a breakpoint line 7
    buf = GPS.EditorBuffer.get(GPS.File("test.adb"))
    buf.current_view().goto(buf.at(7, 1))
    yield hook("location_changed", debounced=True)
    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks(other_than=known_tasks)

    # Start a debugging session
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    # Disable the first breakpoint
    view = Breakpoints_View()
    yield view.select(0)
    GPS.execute_action("debug disable breakpoints")
    yield wait_idle()

    # Run the debugger
    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_DAP_server("stackTrace")

    # Check that we hit only the second breakpoint
    gps_assert(debug.current_file, GPS.File("test.adb"), "Wrong breakpoint reached")
