"""
A regexp that matches the empty string must not raise when the search builds
its occurrence: such a match reports no end position at all, so the
occurrence's exclusive end is its own start and nothing gets selected.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    def selection():
        Start = buf.selection_start()
        End = buf.selection_end()
        return (Start.line(), Start.column(), End.line(), End.column())

    #  Select something first, so that the empty match collapsing the
    #  selection is an observable outcome and not the initial state.
    buf.select(buf.at(2, 1), buf.at(2, 3))
    gps_assert(selection(), (2, 1, 2, 3), "wrong initial selection")

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.FILES_FROM_PROJECT)
    s.regexp.set_active(True)
    s.pattern.set_text("X*")

    GPS.execute_action("find next")
    yield wait_tasks(other_than=known_tasks)

    #  The match is empty, so it selects nothing: the selection collapses
    #  onto the position of the match.
    gps_assert(
        selection(),
        (2, 1, 2, 1),
        "wrong selection after an empty match",
    )
