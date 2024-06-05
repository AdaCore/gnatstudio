"""
This test checks the good behavior of the fuzzy completion scoring
regarding casing. In fuzzy mode, casing should be taken into account
for completion items' sorting.
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

    # Trigger the completion in a fuzzy way, by omitting the '_'
    for ch in "MyVa":
        send_key_event(ord(ch))
        yield timeout(100)

    # Verify that the completion window is there
    pop_tree = get_widget_by_name("completion-view")
    gps_assert(
        pop_tree is not None, True, "The completion window should be open at that point"
    )

    # Verify that completion items' order: 'My_Variable_2' should come first,
    # since it's the closest match for 'MyVa' due to the casing.
    gps_assert(
        dump_tree_model(pop_tree.get_model(), 4),
        ["My_Variable_2", "my_variable_1"],
        "Wrong order for fuzzy completion",
    )

    GPS.execute_action("undo")

    # Trigger the completion in a fuzzy way, by omitting the '_'
    for ch in "myva":
        send_key_event(ord(ch))
        yield timeout(100)

    # Verify that the completion window is there
    pop_tree = get_widget_by_name("completion-view")
    gps_assert(
        pop_tree is not None, True, "The completion window should be open at that point"
    )

    # Verify that completion items' order: 'my_variable_1' should come first,
    # since it's the closest match for 'myva' due to the casing.
    gps_assert(
        dump_tree_model(pop_tree.get_model(), 4),
        [
            "my_variable_1",
            "My_Variable_2",
        ],
        "Wrong order for fuzzy completion",
    )
