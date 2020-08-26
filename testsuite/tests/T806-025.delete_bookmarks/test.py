"""
Test whether a bookmark is deleted with a line.
"""

import GPS
from gs_utils.internal.utils import *
from workflows.promises import known_tasks


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(6, 1))
    GPS.execute_action("bookmark create")
    view = Bookmarks()
    yield view.open_and_yield()
    dump = dump_tree_model(view.treeview.get_model(), 1)
    gps_assert(dump, ['Main (main.adb:6:1)'])

    buf.delete(buf.at(6, 1), buf.at(7, 1))
    yield wait_idle()
    dump = dump_tree_model(view.treeview.get_model(), 1)
    gps_assert(dump, [])
