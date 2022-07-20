"""
This test checks the good behavior of the completion strict mode.
In particular, it tests that we should only match the completion proposals
that start with the given prefix, and not the ones that just contain the
prefix.
"""

import GPS
from gs_utils.internal.utils import *


LABEL_COLUMN = 6


@run_test_driver
def run_test():
    yield wait_tasks()
    GPS.Preference("Smart-Completion-Mode").set("3")
    GPS.Preference("Completion-Filter-Mode").set("Strict")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(5, 1).end_of_line())
    yield wait_idle()

    for ch in ".Te":
        send_key_event(ord(ch))
        yield timeout(100)

    yield wait_until_true(
        lambda: get_widget_by_name("completion-view") != None)

    # Verify that the completion window is there
    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing...")

    gps_assert(pop_tree is not None, True,
               "The completion window should be open at that point")

    # Verify that the invisible 'Do_Nothing' subprogram is listed after
    # the visible one
    gps_assert(
        dump_tree_model(pop_tree.get_model(), LABEL_COLUMN),
        ['Text_IO'],
        "We should only match Text_IO in strict mode.")
