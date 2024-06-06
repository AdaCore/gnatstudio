"""
This test checks that we correctly execute the LSP
commands attached to invisible completion items ('auto-import' in this
case).
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(5, 10).end_of_line())
    yield wait_tasks()

    for ch in "ble":
        send_key_event(ord(ch))
        yield timeout(100)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)

    # Verify that the completion window is there
    pop_tree = get_widget_by_name("completion-view")
    gps_assert(
        pop_tree is not None, True, "The completion window should be open at that point"
    )

    # Select the first entry in the completion window
    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    text = buf.get_chars(buf.at(1, 1), buf.at(2, 1).end_of_line())
    gps_assert(
        text.strip(),
        "with Bar;",
        "The 'auto-import' command for invisible completion"
        + "items has not been executed",
    )
