"""
Modifying a breakpoint should not create a new one.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    yield wait_tasks()
    # Open the editor and add a breakpoint
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = b.current_view()
    view.goto(b.at(4, 1))
    yield wait_idle()
    GPS.execute_action("debug set line breakpoint")
    yield wait_idle()

    GPS.execute_action("open breakpoints editor")
    bp = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, bp.pywidget())[0]
    selection = tree.get_selection()
    model = tree.get_model()
    gps_assert(
        len(dump_tree_model(model)),
        1,
        "Wrong number of breakpoints when opening the view",
    )
    yield wait_idle()

    # Open the Breakpoints view edit dialog
    view = Breakpoints_View()
    yield view.select(0)
    ed = view.edit()
    yield ed.open_and_yield()
    bp_type = get_widget_by_name("breakpoint-type-combo", ed.dialogs)
    gps_assert(bp_type.get_active(), 0, "Check the initial breakpoint type")
    yield ed.ok()
    gps_assert(
        len(dump_tree_model(model)),
        1,
        "Wrong number of breakpoints after editing without debugger",
    )
    yield wait_idle()

    view = Breakpoints_View()
    yield view.select(0)
    ed = view.edit()
    yield ed.open_and_yield()
    bp_type = get_widget_by_name("breakpoint-type-combo", ed.dialogs)
    bp_type.set_active(3)
    yield ed.ok()
    gps_assert(
        len(dump_tree_model(model)),
        1,
        "Wrong number of breakpoints after changing breakpoint type",
    )
    yield wait_idle()

    view = Breakpoints_View()
    yield view.select(0)
    ed = view.edit()
    yield ed.open_and_yield()
    bp_type = get_widget_by_name("breakpoint-type-combo", ed.dialogs)
    bp_type.set_active(0)
    ed.filename.set_text("main.adb")
    ed.line.set_text("4")
    yield ed.ok()
    gps_assert(
        len(dump_tree_model(model)),
        1,
        "Wrong number of breakpoints after changing breakpoint type",
    )
    yield wait_idle()

    # Start the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    gps_assert(
        len(dump_tree_model(model)),
        1,
        "Wrong number of breakpoints when starting the debugger",
    )

    # Run the debuggee
    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_DAP_server("stackTrace")
    yield wait_idle()

    view = Breakpoints_View()
    yield view.select(0)
    ed = view.edit()
    yield ed.open_and_yield()
    bp_type = get_widget_by_name("breakpoint-type-combo", ed.dialogs)
    gps_assert(bp_type.get_active(), 0, "The breakpoint type was not preserved")
    yield ed.ok()
    gps_assert(
        len(dump_tree_model(model)),
        1,
        "Wrong number of breakpoints after editing with debugger",
    )
