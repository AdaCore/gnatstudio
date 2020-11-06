"""
This test checks the good behavior of the fuzzy completion scoring, which is
used to order the completion items that match.
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
    view.goto(buf.at(6, 1).end_of_line())
    yield wait_idle()

    # Trigger the completion in a fuzzy way, by omitting the '_'
    for ch in "MyVa":
        send_key_event(ord(ch))
        yield timeout(100)

    # Verify that the completion window is there
    pop_tree = get_widget_by_name("completion-view")
    gps_assert(pop_tree is not None, True,
               "The completion window should be open at that point")

    # Verify that completion items' order: 'My_Var' should come first,
    # since it's the closest match for 'MyVa'. Then it should be
    # 'My_Varibale' and finally 'My_Second_Var'.
    gps_assert(dump_tree_model(pop_tree.get_model(), 4),
               ['My_Var', 'My_Variable', 'My_Second_Var'],
               "Wrong order for fuzzy completion")
