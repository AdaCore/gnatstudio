"""
Test that we gray out pending breakpoints in the
Breakpoints view.
"""

import GPS
from gs_utils.internal.utils import *


COL_FG_COLOR = 11

EXPECTED_FG_COLORS = "[Gdk.RGBA(red=0.000000, green=0.000000, blue=0.729412, alpha=1.000000), Gdk.RGBA(red=0.000000, green=0.000000, blue=0.376471, alpha=1.000000), Gdk.RGBA(red=0.000000, green=0.000000, blue=0.376471, alpha=1.000000)]"


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
    combo = get_widget_by_name("Breakpoint_Type")
    combo.set_active(1)
    name = get_widget_by_name("Subprogram_Name")
    name.append_text("tmp")
    name.set_active(1)
    yield ed.ok()

    # Create a breakpoint for an exception that does not exist.
    # Should be pending after starting the debugger.
    ed = view.create()
    yield ed.open_and_yield()
    combo = get_widget_by_name("Breakpoint_Type")
    combo.set_active(3)
    name = get_widget_by_name("Exception_Name")
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
    row_fg_colors = str(dump_tree_model(view.list.get_model(), COL_FG_COLOR))
    gps_assert(
        row_fg_colors.strip(),
        EXPECTED_FG_COLORS.strip(),
        "Wrong fg color for rows in the Breakpoints view",
    )
