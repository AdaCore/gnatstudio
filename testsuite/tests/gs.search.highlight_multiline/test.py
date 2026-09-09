"""
A search result whose match runs past the end of its first line is
highlighted from the start of the match to the end of that line: the rest
of the line is part of the match, and the highlighting of a result covers
one line.

The two files hold the same text but for three characters, written outside
ASCII in one of them and inside it in the other. The match is the same in
both, so the highlighting has to be the same: it counts characters of the
line and not the bytes they take.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


#  Matches 'tail' at the end of the first line, its terminator, and the
#  beginning of the second line
PATTERN = "tail\\nbeg"


@run_test_driver
def test_driver():
    GPS.Preference("General-Charset").set("UTF-8")

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.regexp.set_active(True)

    for Name in ("utf8.txt", "ascii.txt"):
        buf = GPS.EditorBuffer.get(GPS.File(Name))
        yield wait_idle()

        s.pattern.set_text(PATTERN)
        GPS.execute_action("find all")
        yield wait_tasks(other_than=known_tasks)

        Line = buf.get_chars(buf.at(1, 1), buf.at(1, 1).end_of_line())
        Last = len(Line.rstrip("\n"))

        #  'AA  ...    tail' : only the last four characters are matched
        gps_assert(
            "".join(
                "X" if buf.at(1, C).get_overlays() else "." for C in range(1, Last + 1)
            ),
            "." * (Last - 4) + "XXXX",
            "wrong highlighting of a multiline match in " + Name,
        )
        gps_assert(
            [O.name() for O in buf.at(1, Last).get_overlays()],
            ["Search results"],
            "wrong style on the multiline match in " + Name,
        )
