"""
This test checks that completion items that match the prefix
with matching characters closer to the start of the prefix are
correctly sorted (i.e: higher score).
"""

import GPS
from gs_utils.internal.utils import *


LABEL_COLUMN = 6


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(8, 1).end_of_line())
    yield wait_idle()

    for ch in "No":
        send_key_event(ord(ch))
        yield timeout(100)

    # Verify that the completion window is there
    yield wait_until_true(
        lambda: get_widget_by_name("completion-view") != None,
        timeout=2000)
    pop_tree = get_widget_by_name("completion-view")
    gps_assert(pop_tree is not None, True,
               "The completion window should be open at that point")

    # Verify that the invisible 'Do_Nothing' subprogram is listed after
    # the visible one
    gps_assert(
        dump_tree_model(pop_tree.get_model(), LABEL_COLUMN),
        ['Nothing', 'Do_Nothing'],
        "Wrong order in completion")
