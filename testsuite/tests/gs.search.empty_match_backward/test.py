"""
Searching backward with a regexp that matches the empty string must not
raise: the callback that looks for the match closest to the cursor has to
treat an empty match as ending where it starts, on every line and not
only on the cursor's own line.

An empty match selects nothing, so the cursor stays where it is and there
is no position to assert on; the expectation of this test is that the
search completes without an exception.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    #  Put the cursor away from the first line, so that the empty matches
    #  on the lines before it are the ones tested against the cursor: it
    #  is that comparison which used to read the end of an empty match.
    buf.current_view().goto(buf.at(4, 1))

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.regexp.set_active(True)
    s.pattern.set_text("X*")

    GPS.execute_action("find previous")
    yield wait_tasks(other_than=known_tasks)
