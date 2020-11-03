"""
This test checks the good behavior of the fuzzy completion filter mode.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_RESULT = "My_Variable"


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    GPS.Preference("Completion-Filter-Mode").set("Fuzzy")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(5, 1).end_of_line())
    yield wait_idle()

    # Trigger the completion
    for ch in "My":
        send_key_event(ord(ch))
        yield timeout(100)

    # Verify that the completion window is there
    pop_tree = get_widget_by_name("completion-view")
    gps_assert(pop_tree is not None, True,
               "The completion window should be open at that point")

    # Now omit the '_' in 'My_Variable' and check if the completion
    # still matches
    for ch in "Var":
        send_key_event(ord(ch))
        yield timeout(100)

    gps_assert(dump_tree_model(pop_tree.get_model(), 4),
               ['My_Variable'],
               "'MyVar' should (only) match 'My_Variable' in fuzzy mode")
