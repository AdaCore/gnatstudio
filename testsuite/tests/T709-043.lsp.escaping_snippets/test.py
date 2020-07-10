"""
This test checks that pressing ESC when the completion window is opened
while expanding LSP snippets close the completion window without exiting
the snippet expansion.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_RESULT = "  Obj.Do_Nothing (Var, 1)"


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(8, 1).end_of_line())
    yield wait_idle()

    # Insert a completion snippet received from clangd
    for ch in "Not":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield timeout(300)

    # Trigger the completion window by typing "Var"
    for ch in "Var":
        send_key_event(ord(ch))
        yield timeout(100)

    line = buf.get_chars(buf.at(8, 1), buf.at(8, 1).end_of_line())
    gps_assert("Obj.Do_Nothing (Var" in line.strip(), True,
               "Var has not been inserted properly")

    pop_tree = get_widget_by_name("completion-view")
    gps_assert(pop_tree != None, True, "The completion window is absent")

    # Press ESC to close the completion window
    send_key_event(GDK_ESCAPE)
    yield timeout(50)

    # Press TAB to go to the next parameter in the snippet expansion
    send_key_event(GDK_TAB)
    yield timeout(50)

    # Type '1' to give a value to the last snippet parameter
    send_key_event(ord('1'))
    yield timeout(50)

    # Verify that the snippet parameters have been inserted properly
    line = buf.get_chars(buf.at(8, 1), buf.at(8, 1).end_of_line())
    gps_assert(line.strip(), EXPECTED_RESULT.strip(),
               "The snippet parameter values have not been inserted properly")
