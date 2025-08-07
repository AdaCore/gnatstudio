"""
Check the selection after ENTER at the end of a line in a GPR file.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("default.gpr"))
    yield wait_tasks()
    start = buf.get_chars(include_hidden_chars=False)
    buf.current_view().goto(buf.at(2, 1).end_of_line())
    pygps.send_key_event(GDK_RETURN)
    yield wait_idle()
    # After ENTER nothing should be selected
    gps_assert(
        buf.selection_start(),
        buf.selection_end(),
        "Wrong selection after enter",
    )
