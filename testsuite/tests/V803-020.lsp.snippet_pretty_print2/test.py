"""
Test the pretty printing of snippet: it should align arrows even for the
already existing parameter.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED = """   Obj.Do_Nothing
     (D             => "Hello",
      AAAAAAAAAAAAA => Integer,
      BBBBBBB       => Float)
end Main;
"""


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_idle()

    # Trigger the parameter completion and select the first result
    send_key_event(ord(','))
    yield wait_until_true(
        lambda: get_widget_by_name("completion-view") is not None)
    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield timeout(500)
    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    gps_assert(buf.get_chars(frm=buf.at(7, 1)),
               EXPECTED,
               "Wrong completion for ',' without newline")

    # Remove completion and ","
    buf.undo()
    buf.undo()
    buf.undo()
    yield wait_idle()
    yield timeout(500)

    # It should still work even with a new line
    buf.insert(buf.at(7, 1).end_of_line(), "\n")
    send_key_event(ord(','))
    yield wait_until_true(
        lambda: get_widget_by_name("completion-view") is not None)
    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield timeout(500)
    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    gps_assert(buf.get_chars(frm=buf.at(7, 1)),
               EXPECTED,
               "Wrong completion for ',' with newline")
