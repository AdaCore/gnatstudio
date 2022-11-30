"""
This test checks that we don't have any completion when
typing within a string.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(3, 1).end_of_line())
    yield wait_idle()

    # Type a character wthin an Ada string: we should not have any results,
    # in particular no alias completion (no 'main (alias)' result for instance)
    for ch in "  mai":
        send_key_event(ord(ch))
        yield timeout(100)

    yield timeout(1000)
    gps_assert(
        get_widget_by_name("completion-view"),
        None,
        "We should not have any completion window",
    )
