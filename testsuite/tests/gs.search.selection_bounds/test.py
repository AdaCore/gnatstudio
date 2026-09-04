"""
'Find next' selects exactly the match. An occurrence's end position is
exclusive, so a one-character match is the boundary case; a match holding or
following multi-byte characters checks that the positions count characters,
and a match at the end of a line checks the end-of-line boundary.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


#  Each pattern must come out selected verbatim
PATTERNS = (
    "X",  # one character
    "Integer",  # several characters
    "café",  # holds a multi-byte character
    "ééé",  # several multi-byte characters
    "Main;",  # ends at the end of a line
    ";",
)  # one character at the end of a line


@run_test_driver
def test_driver():
    GPS.Preference("General-Charset").set("UTF-8")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)

    for Pattern in PATTERNS:
        s.pattern.set_text(Pattern)

        GPS.execute_action("find next")
        yield wait_tasks(other_than=known_tasks)

        Start = buf.selection_start()
        End = buf.selection_end()

        #  The end of the selection is exclusive: the last selected
        #  character is the one just before it.
        gps_assert(
            buf.get_chars(Start, End.forward_char(-1)),
            Pattern,
            "'find next' selected %s:%s..%s:%s for '%s'"
            % (Start.line(), Start.column(), End.line(), End.column(), Pattern),
        )

    GPS.execute_action("Close current window")
