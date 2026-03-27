"""
Create a breakpoint via the action "debug create breakpoint" and then delete
it via the Buffer side column.
"""

import GPS
from gs_utils.internal.utils import *
from workflows import run_as_workflow


@run_test_driver
def test_driver():
    yield wait_tasks()

    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_for_mdi_child("main.adb")

    GPS.execute_action("open breakpoints editor")
    bp = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, bp.pywidget())[0]
    model = tree.get_model()
    gps_assert(
        len(dump_tree_model(model)),
        0,
        "Wrong number of breakpoints when opening the view",
    )

    # Create a breakpoints using the view
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    ed.filename.set_text("main.adb")
    ed.line.set_value(11)
    yield ed.ok()
    yield wait_idle()
    gps_assert(
        len(dump_tree_model(model)),
        1,
        "Wrong number of breakpoints after creating a breakpoint",
    )

    # Delete it using the editor side column
    buf.click_on_line_number(line=11)
    yield wait_idle()
    gps_assert(
        len(dump_tree_model(model)),
        0,
        "Wrong number of breakpoints after deleting the breakpoint"
        + " via a click in the editor side column",
    )
