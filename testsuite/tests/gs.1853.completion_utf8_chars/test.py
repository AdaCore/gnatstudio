"""
Verify that accepting a completion proposal containing UTF-8 multi-byte
characters does not remove the preceding character at the insertion point.
"""

import GPS
from gs_utils.internal.utils import *


LABEL_COLUMN = 6


@run_test_driver
def run_test():
    GPS.Preference("General-Charset").set("UTF-8")
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(6, 1).end_of_line())
    yield wait_tasks()

    # The prefix "Él" contains a multi-byte UTF-8 character
    # (É = 2 bytes). Trigger completion from end of line.
    GPS.execute_action("complete identifier (advanced)")
    yield wait_until_true(lambda: get_widget_by_name("completion-view") is not None)
    yield timeout(300)

    pop_tree = get_widget_by_name("completion-view")
    gps_assert(pop_tree is not None, True, "The completion window should be open")

    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    line = buf.get_chars(buf.at(6, 1), buf.at(6, 1).end_of_line())
    gps_assert(
        "X := Élève_Count" in line,
        True,
        "Completion of UTF-8 identifier should not eat the preceding char."
        + " Got: "
        + line,
    )
