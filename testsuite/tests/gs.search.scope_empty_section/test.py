"""
A search restricted to a lexical scope must not report a match taken from
outside it.

The file starts with a comment, and holds a second one right after the
first: the sections of code around them are empty. An empty section has no
text to match, and a range with nothing in it must not reach the pattern --
handed a start that comes after its end, the regexp engine reports matches
taken from anywhere in the buffer.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


PATTERN = "aaa|bbb|null|zzz"

IN_COMMENTS = [(1, 5), (2, 5)]
OUTSIDE_COMMENTS = [(4, 28), (6, 4)]


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.regexp.set_active(True)
    scope = get_widgets_by_type(Gtk.ComboBox, s.dialog)[2]

    for Name, Expected in (
        ("Comments Only", IN_COMMENTS),
        ("All but Comments", OUTSIDE_COMMENTS),
        ("Whole Text", IN_COMMENTS + OUTSIDE_COMMENTS),
    ):
        select_combo(scope, Name)
        s.pattern.set_text(PATTERN)

        GPS.execute_action("find all")
        yield wait_tasks(other_than=known_tasks)

        gps_assert(
            sorted(
                (M.get_line(), M.get_column())
                for M in GPS.Message.list(category="Search for: " + PATTERN)
            ),
            sorted(Expected),
            "wrong matches with the scope set to '%s'" % Name,
        )
