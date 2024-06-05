"""
This test checks that disabling/enabling the LSP completion snippets
works fine after restarting the language server.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_NO_SNIPPET = "Do_Something"
EXPECTED_SNIPPET = "Do_Something (A : Integer)"


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    GPS.Preference("Completion-Filter-Mode").set("Fuzzy")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_tasks()

    # Trigger completion...
    for ch in "Do_Some":
        send_key_event(ord(ch))
        yield timeout(200)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") is not None)
    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing..."
    )

    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    # Check that we have a snippet
    line = buf.get_chars(buf.at(7, 1), buf.at(7, 1).end_of_line())
    gps_assert(line.strip(), EXPECTED_SNIPPET.strip(), "Snippet should be inserted")

    # Disable completion snippets and restart the ALS
    GPS.execute_action("undo")
    GPS.Preference("LSP-Completion-Use-Snippets").set(False)
    GPS.execute_action("Restart ada language server")
    yield hook("language_server_started")

    # Retrigger completion...
    for ch in "Do_Some":
        send_key_event(ord(ch))
        yield timeout(200)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") is not None)
    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing..."
    )

    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    # Check that we don't have a snippet now
    line = buf.get_chars(buf.at(7, 1), buf.at(7, 1).end_of_line())
    gps_assert(
        line.strip(),
        EXPECTED_NO_SNIPPET.strip(),
        "No snippet should be inserted by default",
    )
