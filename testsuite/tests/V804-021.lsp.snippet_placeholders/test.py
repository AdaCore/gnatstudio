"""
This test checks the placeholders for snippet especially when there
are multiple digits of them.
"""

import GPS
from gs_utils.internal.utils import *


def generate_expected(map_param_value, nb_iteration=10):
    res = ""
    for key, value in map_param_value.items():
        for i in range(1, nb_iteration):
            res = res + "      %s%d => %s,\n" % (key, i, value)
    res = "(" + res[6:-2] + ")"
    return res


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 18))
    yield wait_idle()

    # Trigger the parameter completion
    for c in " (":
        send_key_event(ord(c))
        yield timeout(300)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") is not None)
    pop_tree = get_widget_by_name("completion-view")
    yield wait_until_true(lambda: pop_tree.get_model() != None)
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing..."
    )

    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    # Verify that it has been correctly parsed by the aliases plugin
    line = buf.get_chars(buf.at(7, 19), buf.at(25, 1).end_of_line())
    gps_assert(
        line.strip(),
        generate_expected({"A": "Integer", "B": "Float"}),
        "The completion snippet has not been correctly inserted",
    )
