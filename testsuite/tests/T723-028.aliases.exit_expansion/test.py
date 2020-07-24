"""
This test checks that ALS completion snippets are handled properly
by GNAT Studio.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_SNIPPET = "  Do_Nothing (A : Integer)"
EXPECTED_RESULT = "  Obj.Do_Nothing (1, 2)"


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(6, 1).end_of_line())
    yield wait_idle()

    # Insert a completion snippet received from clangd
    for ch in "ot":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    click_in_tree(pop_tree, path="0", events=double_click_events)

    # Verify that it has been correctly parsed by the aliases plugin
    line = buf.get_chars(buf.at(6, 1), buf.at(6, 1).end_of_line())
    gps_assert(line.strip(), EXPECTED_SNIPPET.strip(),
               "The completion snippet has not been correctly inserted")

    view.goto(buf.at(5, 1))
    yield wait_idle()

    overlays = buf.at(6, 17).get_overlays()
    gps_assert(overlays, [], "There should be no aliases' overlays at this loc")
