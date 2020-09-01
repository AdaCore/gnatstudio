"""
This test checks that the 'replace' completion insert mode
correctly overwrites words when accepting completions.
"""

import GPS
from gs_utils.internal.utils import *

EXPECTED_LINE_1 = """Ada.Text_IO.Put_Line ("Hello World!");"""
EXPECTED_LINE_2 = """Var_1 := Var_2 + Var_2;"""
EXPECTED_END_OF_BUFFER = "Ada\nend Main;\n"


@run_test_driver
def run_test():
    GPS.Preference('Completion-Insert-Mode').set("replace")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(5, 19))
    yield wait_idle()

    # Trigger completion in the middle of 'Put_Line' and select
    # 'Put_Line' in the completion window.

    for ch in "_Li":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    click_in_tree(pop_tree, path="0", events=double_click_events)

    # Verify that the text was actually replaced and not shifted to the
    # right

    line = buf.get_chars(buf.at(5, 1), buf.at(5, 1).end_of_line())
    gps_assert(line.strip(), EXPECTED_LINE_1.strip(),
               "The 'replace' completion insert mode did not work on Put_Line")

    # Trigger completion on "Ad" and select the first result, which should
    # be "Ada": we are not in the middle of any word, so completion should
    # act like if it was in 'insert' mode, without modifying the rest of the
    # buffer

    view.goto(buf.at(13, 1))
    yield wait_idle()

    for ch in "Ada":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    click_in_tree(pop_tree, path="0", events=double_click_events)

    # Verify that the text has been correctly inserted, without modifying
    # the rest of the buffer

    end_of_buffer = buf.get_chars(buf.at(13, 1), buf.end_of_buffer())
    gps_assert(end_of_buffer, EXPECTED_END_OF_BUFFER,
               "The 'replace' completion insert did not work on Var_1")
