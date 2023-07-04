"""
This test checks that we display separate proposals for overloaded
subprograms when snippets are enabled.
"""

import GPS
from gs_utils.internal.utils import *


LABEL_COLUMN = 6


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_tasks(other_than=known_tasks)

    # Insert a completion snippet received from als
    for ch in "Not":
        send_key_event(ord(ch))
        yield timeout(100)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)
    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_idle()

    gps_assert(
        dump_tree_model(model, LABEL_COLUMN),
        ['Do_Nothing', 'Do_Nothing'],
        "We should have two proposals for Do_Nothing since it's overloaded")
