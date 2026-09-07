"""
After replacing a match with text holding multi-byte characters, the position
recorded for the end of the replacement is advanced by characters, not by
UTF-8 bytes. Otherwise the cursor, and the next 'replace and find' position,
land past the end of the replacement.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


REPLACEMENT = "ÉÉÉ"


@run_test_driver
def test_driver():
    GPS.Preference("General-Charset").set("UTF-8")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(1, 1))

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.pattern.set_text("AAA")
    s.replace_text.set_text(REPLACEMENT)

    #  'replace current' replaces the match and leaves the position just
    #  after the replacement, without searching on.
    GPS.execute_action("replace current")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        buf.get_chars(buf.at(3, 1), buf.at(3, 1).end_of_line()),
        "   ÉÉÉ := 1;  --  after AAA\n",
        "the first occurrence was not replaced",
    )

    #  The replacement holds three characters, so it ends on column 7 of the
    #  line; counting its six UTF-8 bytes would land past that.
    Cursor = buf.current_view().cursor()
    gps_assert(
        (Cursor.line(), Cursor.column()),
        (3, 7),
        "the cursor is at %s after the replacement" % Cursor,
    )

    #  The next occurrence is the one later on the same line, and searching
    #  on from the replacement must find all of it.
    GPS.execute_action("find next")
    yield wait_tasks(other_than=known_tasks)

    Start = buf.selection_start()
    End = buf.selection_end()
    gps_assert(
        buf.get_chars(Start, End.forward_char(-1)),
        "AAA",
        "the next occurrence was not selected, got %s:%s..%s:%s"
        % (Start.line(), Start.column(), End.line(), End.column()),
    )

    GPS.execute_action("Close current window")
