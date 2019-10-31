"""
The Traces view should not be broken when reopening it.
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


def open_traces_page():
    # Open the preferences dialog
    p = dialogs.Preferences()
    yield p.open_and_yield()

    # Open the Traces notebook page
    side_tree = get_widgets_by_type(Gtk.TreeView, p.dialog)[0]
    path = find_in_tree(side_tree, 0, "Traces")
    click_in_tree(side_tree, path)

    # Verify the content of the Traces view
    view_tree = get_widget_by_name("Traces editor tree")
    gps_assert(view_tree.get_model().iter_n_children(None) != 0,
               True,
               "The Traces view should not be empty")

    # Close the dialog
    p.dialog.close()


@run_test_driver
def run_test():
    yield open_traces_page()
    yield open_traces_page()
