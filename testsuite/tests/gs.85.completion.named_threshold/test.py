"""
This test check the preference LSP-Ada-Param-Naming-Threshold
(Set via preferences.xml before starting the server).
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_1 = 'Foo (I => Integer, J => Integer)'
EXPECTED_2 = 'Bar (S => String)'


def test_completion(buf, s, expected):
    for c in s:
        send_key_event(ord(c))
        yield timeout(100)
    yield wait_until_true(lambda: get_widget_by_name("completion-view"))
    pop_tree = get_widget_by_name("completion-view")
    click_in_tree(pop_tree, path="0", events=double_click_events)
    gps_assert(buf.get_chars().splitlines()[4],
               expected,
               "Wrong completion for " + s)


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(5, 1).end_of_line())
    yield wait_idle()

    yield test_completion(buf, "Foo", EXPECTED_1)

    # Undo the completion
    buf.undo()
    # Undo Foo
    buf.undo()
    buf.undo()
    buf.undo()

    yield test_completion(buf, "Bar", EXPECTED_2)
