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

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.FILES_FROM_PROJECT)
    s.regexp.set_active(True)
    s.pattern.set_text("X*")

    GPS.execute_action("find next")
    yield wait_tasks(other_than=known_tasks)

    Start = buf.selection_start()
    End = buf.selection_end()

    gps_assert(
        (Start.line(), Start.column()),
        (End.line(), End.column()),
        "an empty match should select nothing, got %s .. %s" % (Start, End),
    )
