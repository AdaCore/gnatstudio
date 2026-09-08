"""
An empty match has no end of its own: it holds no text, so a search result
for one highlights nothing. The character it sits in front of is not part
of the match, and highlighting it would claim a match of one character.

The second pattern matches a single character and does highlight it, so
that the first assertion cannot be met by highlighting nothing at all.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


TEXT = "abc"


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("f.txt"))
    yield wait_idle()
    gps_assert(buf.get_chars(), TEXT + "\n", "wrong contents of the file")

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.regexp.set_active(True)

    def highlighted():
        """The characters of the first line carrying a search highlight"""
        return "".join(
            "X" if buf.at(1, C).get_overlays() else "." for C in range(1, len(TEXT) + 1)
        )

    #  'X*' matches the empty string in front of every character
    s.pattern.set_text("X*")
    GPS.execute_action("find all")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(highlighted(), "." * len(TEXT), "an empty match highlighted text")

    #  A match holding one character highlights that character
    s.pattern.set_text("b")
    GPS.execute_action("find all")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(highlighted(), ".X.", "wrong highlighting of a 'b' match")
