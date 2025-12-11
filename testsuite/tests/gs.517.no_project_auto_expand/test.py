"""
Test that we don't expand any node in the project view when not
 preserve-nodes-state
"""

import GPS
from gs_utils.internal.utils import *

Cnt = 0


def On_Expand(tree_view, path, data):
    global Cnt
    Cnt = Cnt + 1


@run_test_driver
def driver():
    global Cnt
    yield wait_tasks()
    explorer = get_widget_by_name("Project Explorer Tree")
    explorer.map_expanded_rows(On_Expand, None)
    gps_assert(
        Cnt,
        0,
        "Should not be expanded",
    )

    # just check that map_expanded_rows works as expected
    explorer.expand_row(Gtk.TreePath("0"), True)
    explorer.map_expanded_rows(On_Expand, None)
    yield wait_idle()
    gps_assert(
        Cnt > 0,
        True,
        "Should be expanded",
    )
