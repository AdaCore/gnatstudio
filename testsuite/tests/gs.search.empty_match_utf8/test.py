"""
A regexp that matches the empty string matches once at each character, and
a character is not always one byte long. Resuming such a search inside the
encoding of a character would report that character's position again, so
the same match would be found over and over.

Both files hold 'a', an 'e-acute' taking two bytes, 'b' and a line
terminator: four characters, and four matches.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


ACCENTED = "aéb\n"


@run_test_driver
def test_driver():
    GPS.Preference("General-Charset").set("UTF-8")

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.regexp.set_active(True)
    s.pattern.set_text("X*")
    s.replace_text.set_text("@")

    #  One match per character, and no position reported twice
    buf = GPS.EditorBuffer.get(GPS.File("found.txt"))
    yield wait_idle()
    gps_assert(buf.get_chars(), ACCENTED, "the file was not read as UTF-8")

    GPS.execute_action("find all")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        sorted(
            (M.get_line(), M.get_column())
            for M in GPS.Message.list(category="Search for: X*")
        ),
        [(1, 1), (1, 2), (1, 3), (1, 4)],
        "wrong positions for the empty matches",
    )

    #  Each match is replaced once, in front of its own character
    buf = GPS.EditorBuffer.get(GPS.File("replaced.txt"))
    yield wait_idle()

    GPS.execute_action("replace all")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        buf.get_chars(),
        "".join("@" + C for C in ACCENTED),
        "wrong replacement of the empty matches",
    )
