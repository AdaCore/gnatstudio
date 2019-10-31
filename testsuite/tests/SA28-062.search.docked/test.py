"""
The "Search" action behavior changes depending on the focus.
Test it in configuration: docked + incremental.
(This test is similar to S606-042.search.focus,
 except that the Search is docked)
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs

SEARCH_ACTION = "Search"


def verify_loc(view, line, column, msg):
    gps_assert(view.cursor().line(), line, "Line issue: " + msg)
    gps_assert(view.cursor().column(), column, "Column issue: " + msg)


@run_test_driver
def test_driver():
    # Force the incremental mode
    GPS.Preference("Search-Incremental").set(True)

    # Open a file and select some text
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    buf.select(buf.at(2, 4), buf.at(2, 7))
    verify_loc(view, 2, 7, "Wrong location via EditorBuffer")

    # Open the search dialog, it should not change the selection in the buffer
    s = dialogs.Search()
    yield s.open_and_yield(docked=True)
    verify_loc(view, 2, 4, "Wrong location at the start of the search")

    # The focus is in the Search view:
    # the "Search" action should find the next match
    GPS.execute_action(SEARCH_ACTION)
    verify_loc(view, 2, 4, "Wrong location after the second search")

    # Give the focus to the editor and move the cursor
    GPS.MDI.get("main.adb").raise_window()
    yield wait_idle()
    view.goto(buf.at(1, 1))
    yield wait_idle()

    # The cursor has been moved by the user => The "Search" action should not
    # change the cursor location, give the focus to the Search view
    # and preselect the "Find" entry
    GPS.execute_action(SEARCH_ACTION)
    verify_loc(view, 1, 1,
               "Search failed when the cursor is in the editor")
    gps_assert(s.pattern.get_selection_bounds(),
               (0, 3),
               "The entry text should be preselectioned")
