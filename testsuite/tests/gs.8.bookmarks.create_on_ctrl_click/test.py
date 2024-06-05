"""
Test whether bookmarks are correctly create when ctrl-clicking
on editor line numbers.
"""

import GPS
from gs_utils.internal.utils import *
from workflows.promises import known_tasks


@run_test_driver
def test_driver():
    # Simulate a ctrl-click on line 6
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.click_on_line_number(6, hyper_mode=True)
    yield wait_idle()

    # Give a name to the newly created bookmark
    for c in "My_Bookmark":
        yield send_key_event(ord(c))
        yield timeout(100)
    yield send_key_event(GDK_RETURN)
    yield wait_idle()

    # Verify that it's correctly added to the Bookmarks view
    view = Bookmarks()
    yield view.open_and_yield()
    dump = dump_tree_model(view.treeview.get_model(), 1)
    gps_assert(dump, ["My_Bookmark"])
