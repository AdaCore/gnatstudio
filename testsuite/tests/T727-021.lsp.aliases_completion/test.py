"""
This test checks that aliases are correcly listed in the
the completion results when LSP completion is enabled.
"""

import GPS
from gs_utils.internal.utils import *


LABEL_COLUMN = 6


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_tasks()

    for ch in "fo":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing..."
    )

    gps_assert(
        dump_tree_model(pop_tree.get_model(), LABEL_COLUMN),
        [
            "for",
            "for (alias)",
            "For_Testing",
            "Float",
            "function",
            "function (alias)",
            "function_is (alias)",
        ],
        "Wrong order for aliases completion items",
    )

    GPS.execute_action("undo")

    # Verify that aliases are not listed on '.'
    for ch in "Ada.":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing..."
    )

    gps_assert(
        "for (alias)" in dump_tree_model(pop_tree.get_model(), LABEL_COLUMN),
        False,
        "Aliases should not be listed on dot completion",
    )

    send_key_event(GDK_ESCAPE)
    yield wait_idle()

    # Verify that aliases are not listed on dotted names (i.e: when completion
    # is trigerred manually or dinamically within a dotted name)
    send_key_event(ord("t"))
    yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing..."
    )

    gps_assert(
        "task (alias)" in dump_tree_model(pop_tree.get_model(), LABEL_COLUMN),
        False,
        "Aliases should not be listed after dotted names",
    )
