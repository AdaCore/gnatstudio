"""
This test checks that dynamic completion is not triggered
when typing a character that is neither a trigger character
(e.g: '.') or an identifier character.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    view = buf.current_view()
    view.goto(buf.at(3, 1).end_of_line())
    yield wait_tasks()

    # Insert ";" and verify that it did not trigger completion
    send_key_event(ord(";"))
    yield timeout(100)

    completion_tree = get_widget_by_name("completion-view")
    gps_assert(
        completion_tree,
        None,
        "The completion window should not be shown after typing ';'",
    )
