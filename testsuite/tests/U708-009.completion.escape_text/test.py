"""
This test verifies that completion items' labels and doc are
properly escaped.
This test should fail due to Gtk warnings if the items are not
properly escaped
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_SNIPPET = "  Obj.Do_Nothing (A : Integer, B : Integer)"
EXPECTED_RESULT = "  Obj.Do_Nothing (1, 2)"


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(12, 1).end_of_line())
    yield wait_idle()

    # Popup the completion view by adding '.'
    send_key_event(ord("."))
    yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing...")

    for i in range(1, 10):
        send_key_event(GDK_DOWN)
        yield timeout(100)
