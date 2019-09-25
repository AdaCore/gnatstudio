"""
No freeze when opening the Assembly view with code compiled without debug
information.
"""
import GPS
from gps_utils.internal.utils import *

COLUMN = 3
WARNING = "['<b>Couldn&apos;t</b> get assembly code']"


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    GPS.execute_action("open assembly view")
    yield wait_for_mdi_child("Assembly")
    yield wait_idle()

    view = GPS.MDI.get("Assembly")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    # The strip() is here to remove the newline
    gps_assert(str(dump_tree_model(model, COLUMN)).strip(),
               WARNING,
               "Missing warning in the Assembly view")
