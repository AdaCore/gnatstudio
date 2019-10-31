"""
Verify that selecting a row in the Outline view properly find/select it in
the editor.
"""

from GPS import *
from gs_utils.internal.utils import *


def verify_loc(tree, button, path, expected, msg):
    click_in_tree(tree, path)
    yield wait_idle()
    gps_assert(button.get_label(),
               "(1 line, 1 char) " + expected,
               "Issue when selecting " + msg)


@run_test_driver
def run_test():
    GPS.execute_action("open Outline")
    buf = GPS.EditorBuffer.get(GPS.File("a.ads"))
    b = get_widget_by_name("Status Bar Cursor Location")
    outline_view = GPS.MDI.get("Outline")
    t = get_widgets_by_type(Gtk.TreeView, outline_view.pywidget())[0]
    # Wait for the Outline view to be filled
    yield hook("semantic_tree_updated")
    yield wait_idle()
    yield verify_loc(t, b, "0:0", "3:10", "a type decl")
    yield verify_loc(t, b, "0:1", "5:15", "a constant decl")
    yield verify_loc(t, b, "0:2", "7:5", "a procedure decl")
    yield verify_loc(t, b, "0:3", "9:5", "the first variable of a multi decl")
    yield verify_loc(t, b, "0:4", "9:8", "the second variable of a multi decl")
    # Does this one last because when opening the file A is selected in the
    # Outline and clicking on a selected row does nothing
    yield verify_loc(t, b, "0", "1:10", "a package decl")
