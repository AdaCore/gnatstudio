"""
Selecting a row in the Outline view should open the file if needed.
This happens when filling the Outline view using the project view.
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
    # retrieve the Outline tree
    GPS.execute_action("open Outline")
    outline_view = GPS.MDI.get("Outline")
    outline_tree = get_widgets_by_type(
        Gtk.TreeView, outline_view.pywidget())[0]

    # retrieve the project tree
    GPS.execute_action("open Project")
    project_view = GPS.MDI.get("Project")
    project_tree = get_widgets_by_type(
        Gtk.TreeView, project_view.pywidget())[0]

    select_in_tree(project_tree, 1, "foo.adb")
    yield wait_idle()

    gps_assert(GPS.MDI.get("foo.adb"), None, "The file should not be opened")
    # Click in the first row ... (a double click is necessary because the
    # node is already selected)
    click_in_tree(outline_tree, "0", events=pygps.double_click_events)
    # ... it should open a file => one more mdi_window
    gps_assert(GPS.MDI.get("foo.adb") != None,
               True,
               "The file should have been opened")
