"""
Verify that selecting a row in the Outline view properly find/select it in
the editor especially inside a generic package.
"""

from GPS import *
from gs_utils.internal.utils import *


def verify_loc(tree, button, path, expected_len, expected_loc, msg):
    click_in_tree(tree, path)
    yield wait_idle()
    gps_assert(
        button.get_label(),
        "(1 line, %s) %s"
        % (
            str(expected_len) + " chars"
            if expected_len > 1
            else str(expected_len) + " char",
            expected_loc,
        ),
        "Issue when selecting " + msg,
    )
    # Verify that the selected row was not changed when jumping in the editor
    gps_assert(
        tree.get_selection().get_selected_rows()[1][0].to_string(),
        path,
        "Path changed after selecting: " + msg,
    )


@run_test_driver
def run_test():
    GPS.execute_action("open Outline")
    buf = GPS.EditorBuffer.get(GPS.File("pack.ads"))
    b = get_widget_by_name("Status Bar Cursor Location")
    outline_view = GPS.MDI.get("Outline")
    t = get_widgets_by_type(Gtk.TreeView, outline_view.pywidget())[0]
    # Wait for the Outline view to be filled
    yield hook("semantic_tree_updated")
    yield wait_idle()
    yield verify_loc(t, b, "0:0", 1, "2:10", "generic T type decl")
    yield verify_loc(t, b, "0:1", 3, "7:16", "Get function decl")
    # Does this one last because when opening the file Pack is selected in the
    # Outline and clicking on a selected row does nothing
    yield verify_loc(t, b, "0", 4, "3:13", "Pack package decl")
