"""
This test verifies that invisible symbols are correctly sorted
(i.e: always below visible symbols that match the same way)
when LSP completion is active.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    GPS.Preference("Completion-Filter-Mode").set("Fuzzy")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_tasks(other_than=known_tasks)

    # Trigger the completion in a fuzzy way, by omitting the '_'
    for ch in "DoSome":
        send_key_event(ord(ch))
        yield timeout(100)

    # Verify that the completion window is there

    yield wait_until_true(
        lambda: get_widget_by_name("completion-view") is not None,
        timeout=1000,
        error_msg="Completion popup did not appear",
    )
    pop_tree = get_widget_by_name("completion-view")
    yield wait_until_true(
        lambda: pop_tree.get_model() is not None,
        timeout=1000,
        error_msg="Completion model not ready yet",
    )

    # Verify that completion items' order: 'Do_Something' (the visible one)
    # should come first.
    gps_assert(
        dump_tree_model(pop_tree.get_model(), 6),
        ["Do_Something", "Do_Someth1ng (invisible)"],
        "Wrong order for invisible symbols completion",
    )
