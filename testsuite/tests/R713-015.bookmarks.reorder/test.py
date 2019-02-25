"""Add bookmarks at the end of the list, then reorder them with API
"""
from GPS import *
from gps_utils.internal.utils import *
from gps_utils.internal.dialogs import *

expect = ['Hello (hello.adb:9:1)', 'Nested (hello.adb:6:1)']


@run_test_driver
def run_test():
    GPS.Preference("bookmark-editor-add-to-end").set(True)
    # Open the file
    buf = GPS.EditorBuffer.get(GPS.File("hello.adb"))

    # Create 2 bookmarks and check Bookmarks model content
    buf.current_view().goto(buf.at(9, 1))
    execute_action("bookmark create")
    buf.current_view().goto(buf.at(6, 1))
    execute_action("bookmark create")
    view = Bookmarks()
    yield view.open_and_yield()
    dump = dump_tree_model(view.treeview.get_model(), 1)
    gps_assert(dump, expect)

    # Place first bookmark after second and check again
    marks = [GPS.Bookmark.get(x) for x in expect]
    marks[0].reorder(after=marks[1])
    dump = dump_tree_model(view.treeview.get_model(), 1)
    gps_assert(dump, list(reversed(expect)))
