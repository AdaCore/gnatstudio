"""
A regexp match can run through a line terminator. The range to replace then
ends at the first position of the following line, and not one column
further on the line holding the terminator: replacing such a match must
consume the terminator and join the two lines.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


#  'is' at the end of the first line, together with its line terminator
PATTERN = "is\\n"

JOINED = "procedure %s @   X : Integer := 0;\n" "begin\n" "   X := 1;\n" "end %s;\n"


@run_test_driver
def test_driver():
    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.regexp.set_active(True)
    s.pattern.set_text(PATTERN)
    s.replace_text.set_text("@")

    #  'Replace all'
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("replace all")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        buf.get_chars(),
        JOINED % ("Main", "Main"),
        "'replace all' over a match ending with a line terminator",
    )

    #  'Replace', on the match the search is currently on
    buf = GPS.EditorBuffer.get(GPS.File("other.adb"))
    buf.current_view().goto(buf.at(1, 1))

    GPS.execute_action("find next")
    yield wait_tasks(other_than=known_tasks)
    yield s.yield_replace()
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        buf.get_chars(),
        JOINED % ("Other", "Other"),
        "'replace' of a match ending with a line terminator",
    )
