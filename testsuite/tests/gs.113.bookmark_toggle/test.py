"""
Advance test for CTRL+Clicking in the editor sidebar for Bookmarks:
- if not bookmark => create one
- if one bookmark => remove it
- if multiple bookmarks => do nothing
"""

import GPS
from gs_utils.internal.utils import *
from workflows.promises import known_tasks

LINE_WITH_BOOKMARKS = 2
LINE_WITH_TOGGLING_BOOKMARK = 6


@run_test_driver
def test_driver():
    view = Bookmarks()
    yield view.open_and_yield()

    gps_assert(dump_tree_model(view.treeview.get_model(), 1),
               ['b1', 'b2'],
               "Missing Bookmarks from xml")

    # Simulate a ctrl-click on line 6
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.click_on_line_number(LINE_WITH_TOGGLING_BOOKMARK, hyper_mode=True)
    yield wait_idle()

    # Give a name to the newly created bookmark
    for c in "b3":
        yield send_key_event(ord(c))
    yield send_key_event(GDK_RETURN)

    # Verify that it's correctly added to the Bookmarks view
    gps_assert(dump_tree_model(view.treeview.get_model(), 1),
               ['b3', 'b1', 'b2'],
               "Bookmark was not created")

    # Clicking on a line with multiple bookmarks should do nothing
    buf.click_on_line_number(LINE_WITH_BOOKMARKS, hyper_mode=True)
    gps_assert(dump_tree_model(view.treeview.get_model(), 1),
               ['b3', 'b1', 'b2'],
               "This click should be ignored")

    # Clicking on a line with one bookmark should remove it
    buf.click_on_line_number(LINE_WITH_TOGGLING_BOOKMARK, hyper_mode=True)
    gps_assert(dump_tree_model(view.treeview.get_model(), 1),
               ['b1', 'b2'],
               "Failed to disable a bookmark via a click")
