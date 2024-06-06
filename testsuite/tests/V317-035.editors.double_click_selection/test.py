"""
Double-clicking on an editor should select the current word, and put
the insert cursor at the end of it.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("a.adb"))
    buf.current_view().goto(buf.at(6, 19))
    yield wait_idle()

    click_in_text(
        GPS.EditorBuffer.get().current_view().cursor(),
        button=1,
        events=pygps.double_click_events,
    )

    gps_assert(
        buf.current_view().cursor(),
        buf.at(6, 24),
        "Cursor not correctly placed when selecting word with double-click",
    )
