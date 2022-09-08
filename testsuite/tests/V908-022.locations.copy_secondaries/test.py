"""
Test the action "locations copy to clipboard" with secondaries messages
"""

from GPS import *
from gs_utils.internal.utils import *

EXPECTED = (
    """p.adb:4:14: error: not type conformant with declaration at p.ads:4
p.ads:4:1: error: not type conformant with declaration at p.ads:4
p.adb:4:14: error: too few parameters""")


def get_clipboard_content():
    return GPS.Clipboard.contents()[GPS.Clipboard.current()]


@run_test_driver
def run_test():
    GPS.File("p.adb").compile()
    tree = pygps.get_widgets_by_type(
        Gtk.TreeView, GPS.MDI.get("Locations").pywidget())[0]
    tree.expand_all()
    yield wait_idle()
    selection = tree.get_selection()
    selection.unselect_all()
    selection.select_path("0:0:0")
    GPS.execute_action("locations copy to clipboard")
    gps_assert(get_clipboard_content(),
               EXPECTED,
               "Issue with copy to clipboard with secondaries messages")
