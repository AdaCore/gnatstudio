"""
'Replace all' with a regexp that matches the empty string must insert each
replacement at the position of its own match.

Every position of the searched range holds an empty match, so the whole
buffer is rewritten and any mislocated match shows up immediately. The
searched range is the whole buffer in the first case and a single lexical
section in the second: a section ends before the end of the buffer, which
is the case where a match's line and column could not be computed.

Neither range reports an empty match just after its last character, so the
end of the buffer keeps nothing after it.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


SOURCE = "procedure %s is\n   --  a comment\nbegin\n   null;\nend %s;\n"


def marked(Text):
    """Text with a '@' inserted in front of each of its characters"""
    return "".join("@" + C for C in Text)


@run_test_driver
def test_driver():
    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.regexp.set_active(True)
    s.pattern.set_text("X*")
    s.replace_text.set_text("@")

    scope = get_widgets_by_type(Gtk.ComboBox, s.dialog)[2]

    #  The whole buffer: every position of it holds an empty match
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    select_combo(scope, "Whole Text")

    yield s.yield_replace_all()
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        buf.get_chars(),
        marked(SOURCE % ("Main", "Main")),
        "'replace all' over the whole buffer",
    )

    #  Comments only: the searched range now ends before the end of the
    #  buffer, and only the comment's own text may be rewritten
    buf = GPS.EditorBuffer.get(GPS.File("other.adb"))
    select_combo(scope, "Comments Only")

    yield s.yield_replace_all()
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        buf.get_chars(),
        "procedure Other is\n   --"
        + marked("  a comment")
        + "\nbegin\n   null;\nend Other;\n",
        "'replace all' restricted to comments",
    )
