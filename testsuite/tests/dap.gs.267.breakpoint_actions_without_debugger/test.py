"""
Test the Breakpoints view actions without a debugger.
"""
import GPS
from gs_utils.internal.utils import *

ENABLED_COLUMN = 0
LINE_COLUMN = 5


@run_test_driver
def test_driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = b.current_view()

    # Create some breakpoints
    for i in range(5, 9):
        view.goto(b.at(i, 1))
        yield wait_idle()
        GPS.execute_action("debug set line breakpoint")
        yield wait_idle()

    # Open the Breakpoints view and check that the breakpoints has been set
    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child("Breakpoints")
    bp = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, bp.pywidget())[0]
    selection = tree.get_selection()
    model = tree.get_model()
    gps_assert(
        dump_tree_model(model, LINE_COLUMN),
        [" 5", " 6", " 7", " 8"],
        "Failed to add breakpoints",
    )

    # Test the disable breakpoints action
    selection.unselect_all()
    selection.select_path(0)
    selection.select_path(1)
    yield wait_idle()
    GPS.execute_action("debug disable breakpoints")
    yield wait_idle()
    gps_assert(
        dump_tree_model(model, ENABLED_COLUMN),
        [False, False, True, True],
        "Failed to disable breakpoints",
    )

    # Test the enable breakpoints action
    selection.unselect_all()
    selection.select_path(1)
    selection.select_path(2)
    yield wait_idle()
    GPS.execute_action("debug enable breakpoints")
    yield wait_idle()
    gps_assert(
        dump_tree_model(model, ENABLED_COLUMN),
        [False, True, True, True],
        "Failed to enable breakpoints",
    )

    # Test the view breakpoint action
    selection.unselect_all()
    selection.select_path(0)
    yield wait_idle()
    GPS.execute_action("debug view breakpoint")
    yield wait_idle()
    gps_assert(view.cursor().line(), 5, "Failed to goto breakpoint")

    # Test the delete breakpoint action
    selection.unselect_all()
    selection.select_path(0)
    selection.select_path(2)
    yield wait_idle()
    GPS.execute_action("debug delete breakpoint")
    yield wait_idle()
    gps_assert(
        dump_tree_model(model, LINE_COLUMN),
        [" 6", " 8"],
        "Failed to delete breakpoints",
    )
