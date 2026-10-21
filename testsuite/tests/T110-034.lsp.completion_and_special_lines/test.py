"""
This test checks that ALS completion snippets sent as pretty printed text edits
are handled properly by GNAT Studio when the buffer contains special lines.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_SNIPPET = "Obj.Do_Nothing (A : Integer, B : Integer)"
EXPECTED_RESULT = "Obj.Do_Nothing (1, 2)"


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    original = buf.get_chars()

    buf.add_special_line(7, "This is a special line")

    view.goto(buf.at(8, 1).end_of_line())
    yield wait_tasks(other_than=known_tasks)

    # Insert a completion snippet received from ALS
    for ch in "Not":
        send_key_event(ord(ch))
        yield timeout(200)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)

    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing..."
    )

    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    # Verify that the text edit preserved the surrounding source and that the
    # snippet has been correctly parsed by the aliases plugin.
    gps_assert(
        buf.get_chars(),
        original.replace("Obj.Do_", EXPECTED_SNIPPET),
        "The completion snippet has not been correctly inserted",
    )

    # Iterate over the snippet params using TAB and give a value to
    # each of them
    for ch in "12":
        send_key_event(ord(ch))
        yield timeout(50)
        send_key_event(GDK_TAB)
        yield timeout(50)

    # Verify that the snippet parameters have been inserted properly
    gps_assert(
        buf.get_chars(),
        original.replace("Obj.Do_", EXPECTED_RESULT),
        "The snippet parameter values have not been inserted properly",
    )

    # Verify that we jumped to the final tab stop
    gps_assert(
        view.cursor(),
        buf.at(8, 1).end_of_line(),
        "last TAB did not jump to the snippet final tab stop",
    )
