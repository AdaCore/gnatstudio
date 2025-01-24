"""
Check that the `Print variable` works.
"""

import GPS
from gs_utils.internal.utils import *
import re
import workflows
from workflows import promises

VALUE = '"this_kind"'


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(9, 1))
    GPS.execute_action("debug set line breakpoint")

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    p = promises.DebuggerWrapper(GPS.File("main"))
    debug = p.get()
    yield p.send_promise("run")
    yield wait_until_not_busy(debug)

    buf.current_view().goto(buf.at(6, 7))
    select_editor_contextual("Debug/Print My_Var")
    yield wait_until_not_busy(debug)

    console = debug.get_console()
    text = console.get_text()
    gps_assert(
        VALUE in text,
        True,
        "No value after 'Print My_Var':" + text,
    )
