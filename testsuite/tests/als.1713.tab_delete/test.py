"""
Verify that tabs can be deleted without corrupting the ALS document.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))
    outline_view = GPS.MDI.get("Outline")
    t = get_widgets_by_type(Gtk.TreeView, outline_view.pywidget())[0]
    yield hook("semantic_tree_updated")
    yield wait_idle()

    expected = dump_tree_model(t.get_model(), 1)

    # Deleting a backspace should not change the Outline content
    b.delete(b.at(2, 1), b.at(2, 4))
    yield hook("semantic_tree_updated")

    gps_assert(
        dump_tree_model(t.get_model(), 1),
        expected,
        "Wrong outline after deleting tab",
    )
