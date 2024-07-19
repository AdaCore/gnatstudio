"""
Test that we gray out pending breakpoints in the
Breakpoints view.
"""

import GPS
from gs_utils.internal.utils import *


COL_FG_COLOR = 10

EXPECTED_FG_COLOR = (
    "Gdk.RGBA(red=0.000000, green=0.729412, blue=0.729412, alpha=1.000000)"
)


@run_test_driver
def test_driver():
    GPS.Preference("Debugger-Pending-Breakpoints").set(False)
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_tasks()

    view = Breakpoints_View()

    # Create one real breakpoint, that actually matches
    # a real SLOC
    ed = view.create()
    yield ed.open_and_yield()
    ed.filename.set_text("main.adb")
    ed.line.set_text("5")
    yield ed.ok()

    # Create a breakpoint for a subprogram that do not exist.
    # Should be pending after starting the debugger.
    ed = view.create()
    yield ed.open_and_yield()
    combo = get_widget_by_name("breakpoint-type-combo", ed.dialogs)
    combo.set_active(1)
    name = get_widget_by_name("breakpoint-subprogram-name", ed.dialogs)
    name.append_text("tmp")
    name.set_active(1)
    yield ed.ok()

    # Create a breakpoint for an exception that does not exist.
    # Should be pending after starting the debugger.
    ed = view.create()
    yield ed.open_and_yield()
    combo = get_widget_by_name("breakpoint-type-combo", ed.dialogs)
    combo.set_active(3)
    name = get_widget_by_name("breakpoint-exception-name", ed.dialogs)
    name.append_text("tmp")
    name.set_active(3)
    yield ed.ok()

    # Build the executable and start debugging it
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_DAP_server("stackTrace")

    view = Breakpoints_View()
    yield wait_idle()

    # Check that pending breakpoints have been grayed out
    model = view.list.get_model()
    iter = model.get_iter("0")
    gps_assert(
        str(model.get_value(iter, COL_FG_COLOR)).strip(),
        EXPECTED_FG_COLOR.strip(),
        "Wrong fg color for rows in the Breakpoints view",
    )
