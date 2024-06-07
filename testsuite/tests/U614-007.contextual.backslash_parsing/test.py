"""
This test verifies that custom contextual menu labels that contain '/'
are correctly displayed.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    view = MDI.get("Project")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    win = Gtk.Window.list_toplevels()
    click_in_tree(tree, path=(0, 1), button=3)

    contextual = dump_contextual(win)
    GPS.Console().write(str(dump_contextual(win)))
    gps_assert(
        "Custom" in contextual and ["Something/Something"] in contextual,
        True,
        "Wrong custom contextual menu label when containing '/'",
    )
