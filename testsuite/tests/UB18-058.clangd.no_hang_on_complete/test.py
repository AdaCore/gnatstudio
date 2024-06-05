"""
This test checks that completion does not hang when clangd
returns completion results that have labels containing unicode
glyphs, which differ from their filter text.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_RESULT = "  std::false_type"


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    GPS.Preference("Completion-Filter-Mode").set("Fuzzy")
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    view = buf.current_view()
    view.goto(buf.at(10, 1).end_of_line())
    yield wait_tasks()

    for ch in ":a":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    click_in_tree(pop_tree, path="0", events=double_click_events)
