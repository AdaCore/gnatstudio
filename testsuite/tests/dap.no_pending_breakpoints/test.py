"""
Test that we gray out pending breakpoints in the
Breakpoints view.
"""

import GPS
from gs_utils.internal.utils import *
import math

COL_FG_COLOR = 10


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

    # Run the executable and wait for the 'debugger_breakpoints_changed' hook
    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_DAP_server("stackTrace")

    view = Breakpoints_View()
    yield wait_idle()

    # Check that pending breakpoints have been grayed out, by checking
    # that the foreground color is not white (default fg color)
    model = view.list.get_model()
    iter = model.get_iter("2")
    fg_color = model.get_value(iter, COL_FG_COLOR)
    gps_assert(
        math.isclose(
            fg_color.blue,
            0.0,
        ),
        False,
        "Wrong fg color for rows in the Breakpoints view, actual color is: %s"
        % str(fg_color),
    )
