"""
Verify that we export only the messages that are currently visible
in the Locations view.
"""

from GPS import *
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    location_view = GPS.MDI.get("Locations").pywidget()
    gps_assert(location_view is not None, True, "Can't find Location View")
    tree = get_widgets_by_type(Gtk.TreeView, location_view)[0]
    tree.get_selection().select_all()

    yield idle_modal_dialog(
        lambda: GPS.execute_action("Locations export to text file"))

    dialog = get_window_by_title("Select a file", Gtk.Window.list_toplevels())
    ent = get_widgets_by_type(Gtk.Entry, dialog)[0]
    ent.set_text("messages.txt")
    get_stock_button(dialog, Gtk.STOCK_OK).clicked()
