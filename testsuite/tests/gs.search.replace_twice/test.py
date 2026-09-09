"""
Replacing a match leaves no current match behind, so a second 'replace
current' with no search in between has nothing to act on.

The position just after a replacement cannot stand for the match: it is the
start of the match itself when the replacement is empty, and the first
position of the next line when the replacement ends with a line terminator.
Neither is a position inside the replaced text, and deriving one by
stepping back a column from it lands on column 0.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)

    #  An empty replacement: the match starts at column 1, so the position
    #  just after the replacement is column 1 as well
    buf = GPS.EditorBuffer.get(GPS.File("empty.txt"))
    buf.current_view().goto(buf.at(1, 1))
    yield wait_idle()

    s.pattern.set_text("aaa")
    s.replace_text.set_text("")

    GPS.execute_action("find next")
    yield wait_tasks(other_than=known_tasks)
    GPS.execute_action("replace current")
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.get_chars(), " bbb\n", "the match was not removed")

    GPS.execute_action("replace current")
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.get_chars(), " bbb\n", "the second 'replace' changed the buffer")

    #  The replacement is taken from the match, terminator included, so it
    #  is text ending with a line terminator
    buf = GPS.EditorBuffer.get(GPS.File("newline.txt"))
    buf.current_view().goto(buf.at(1, 1))
    yield wait_idle()

    s.regexp.set_active(True)
    s.pattern.set_text("aaa (bbb\\n)")
    s.replace_text.set_text("\\1")

    GPS.execute_action("find next")
    yield wait_tasks(other_than=known_tasks)
    GPS.execute_action("replace current")
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.get_chars(), "bbb\nccc\n", "the match was not replaced")

    GPS.execute_action("replace current")
    yield wait_tasks(other_than=known_tasks)
    gps_assert(
        buf.get_chars(),
        "bbb\nccc\n",
        "the second 'replace' changed the buffer",
    )
