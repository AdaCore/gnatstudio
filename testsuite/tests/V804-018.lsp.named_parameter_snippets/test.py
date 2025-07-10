"""
This test checks that ALS parameter completion snippets
work fine when using named notation.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_SNIPPET = "  Obj.Do_Nothing (A => Integer, B => Integer, C => Integer)"
EXPECTED_RESULT = "  Obj.Do_Nothing (A => 1, B => 2, C => 3)"


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_idle()

    # Trigger the parameter completion
    for c in " (":
        send_key_event(ord(c))
        yield timeout(300)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)
    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing..."
    )

    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    # Verify that it has been correctly parsed by the aliases plugin
    line = buf.get_chars(buf.at(7, 1), buf.at(7, 1).end_of_line())
    GPS.Console().write(line.strip())
    gps_assert(
        line.strip(),
        EXPECTED_SNIPPET.strip(),
        "The completion snippet has not been correctly inserted",
    )

    # Iterate over the snippet params using TAB and give a value to
    # each of them
    for ch in "123":
        send_key_event(ord(ch))
        yield wait_idle()
        yield wait_until_true(
            lambda: GPS.Action("toggle to next alias field").can_execute()
        )
        GPS.execute_action("toggle to next alias field")
        yield wait_idle()

    # Verify that the snippet parameters have been inserted properly
    line = buf.get_chars(buf.at(7, 1), buf.at(7, 1).end_of_line())
    gps_assert(
        line.strip(),
        EXPECTED_RESULT.strip(),
        "The snippet parameter values have not been inserted properly",
    )

    # Verify that we jumped to the final tab stop
    gps_assert(
        view.cursor(),
        buf.at(7, 1).end_of_line(),
        "last TAB did not jump to the snippet final tab stop",
    )
