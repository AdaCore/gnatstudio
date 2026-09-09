"""
'Find next' selects exactly the match. An occurrence's end position is
exclusive, so a one-character match is the boundary case; a match holding or
following multi-byte characters checks that the positions count characters,
and a match at the end of a line checks the end-of-line boundary.

A regexp match can also run through a line terminator. The position just
after such a match is the first one of the next line, and not an extra column
on the line holding the terminator.
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

    #  Regexp matches that run through a line terminator. The pattern is
    #  written with an escaped newline; the expected selection is the text it
    #  matches.
    s.regexp.set_active(True)

    for Pattern, Expected in (
        ("Integer := 0;\\n", "Integer := 0;\n"),
        ("and .*\\nbegin", "and \u00e9\u00e9\u00e9\nbegin"),
        ("end Main;\\n", "end Main;\n"),
    ):
        buf.current_view().goto(buf.at(1, 1))
        s.pattern.set_text(Pattern)

        GPS.execute_action("find next")
        yield wait_tasks(other_than=known_tasks)

        Start = buf.selection_start()
        End = buf.selection_end()

        gps_assert(
            buf.get_chars(Start, End.forward_char(-1)),
            Expected,
            "'find next' selected %s:%s..%s:%s for the regexp '%s'"
            % (Start.line(), Start.column(), End.line(), End.column(), Pattern),
        )

    GPS.execute_action("Close current window")
