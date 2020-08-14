"""
This test checks that ALS completion snippets are handled properly
by GNAT Studio.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_SNIPPET = "  Obj.Do_Nothing (A : Integer, B : Integer)"
EXPECTED_RESULT = "  Obj.Do_Nothing (1, 2)"


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_idle()

    # Insert a completion snippet received from clangd
    for ch in "Not":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    click_in_tree(pop_tree, path="0", events=double_click_events)

    # Verify that it has been correctly parsed by the aliases plugin
    line = buf.get_chars(buf.at(7, 1), buf.at(7, 1).end_of_line())
    gps_assert(line.strip(), EXPECTED_SNIPPET.strip(),
               "The completion snippet has not been correctly inserted")

    # Iterate over the snippet params using TAB and give a value to
    # each of them
    for ch in "12":
        send_key_event(ord(ch))
        yield wait_idle()
        GPS.execute_action("toggle to next alias field")
        yield wait_idle()

    # Verify that the snippet parameters have been inserted properly
    line = buf.get_chars(buf.at(7, 1), buf.at(7, 1).end_of_line())
    gps_assert(line.strip(), EXPECTED_RESULT.strip(),
               "The snippet parameter values have not been inserted properly")

    # Verify that we jumped to the final tab stop
    gps_assert(view.cursor(), buf.at(7, 1).end_of_line(),
               "last TAB did not jump to the snippet final tab stop")
