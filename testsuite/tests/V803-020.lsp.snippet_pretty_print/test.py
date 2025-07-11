"""
Test the pretty printing of snippet: it should align arrows for params.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED = """   Obj.Do_Nothing (AAAAAAAAAAAAA => Integer, BBBBBBB => Float, D => String)
end Main;
"""


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_tasks(other_than=known_tasks)

    # Trigger the parameter completion
    for c in " (":
        send_key_event(ord(c))
        yield timeout(300)

    yield wait_until_true(
        lambda: get_widget_by_name("completion-view") is not None,
        timeout=3000,
        error_msg="Timeout exceeded while waiting for completion view to appear"
    )
    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_idle()

    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    gps_assert(
        buf.get_chars(frm=buf.at(7, 1)),
        EXPECTED,
        "Wrong completion when for open parenthese",
    )
