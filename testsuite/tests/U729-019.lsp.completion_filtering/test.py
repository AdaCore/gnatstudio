"""
This test checks that LSP filterText is correctly taken into account
to filter completion results.
"""

import GPS
from gs_utils.internal.utils import *


LABEL_COLUMN = 6


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    GPS.Preference("Completion-Filter-Mode").set("Fuzzy")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_idle()

    # Try to match the '(invisible)' part of the 'Do_Nothing', in
    # fuzzy mode.
    for ch in "Do_inv":
        send_key_event(ord(ch))
        yield timeout(100)

    # Verify that the completion window is there
    pop_tree = get_widget_by_name("completion-view")
    gps_assert(pop_tree is not None, True,
               "The completion window should be open at that point")

    # Verify that we don't match the invisible 'Do_Nothing' subprogram
    gps_assert(
        dump_tree_model(pop_tree.get_model(), LABEL_COLUMN),
        ['Do_inv'],
        "'(invisible)' should not be matched")
