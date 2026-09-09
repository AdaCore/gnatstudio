"""
A Backspace with the cursor on the first column of a line has no character
before it on that line: the position of the deletion is on the previous line,
and computing it must not build an out of range character index.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()

    #  Column 1 of line 2: the deleted character is the line terminator of
    #  line 1, so the two lines get joined.
    view.goto(buf.at(2, 1))
    yield wait_idle()
    send_key_event(GDK_BACKSPACE)
    yield wait_idle()

    gps_assert(
        buf.get_chars(),
        "procedure Main isbegin\n   null;\nend Main;\n",
        "Backspace on the first column did not join the lines",
    )

    #  In the middle of a line it removes the previous character
    view.goto(buf.at(2, 4))
    yield wait_idle()
    send_key_event(GDK_BACKSPACE)
    yield wait_idle()

    gps_assert(
        buf.get_chars(),
        "procedure Main isbegin\n  null;\nend Main;\n",
        "Backspace did not remove the previous character",
    )
