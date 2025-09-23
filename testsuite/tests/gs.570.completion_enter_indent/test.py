"""
Verify that we don't indent 2 times when pressing
enter while the completion window is shown.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Editor-On-Type-Formatter-ada").set("LSP")
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    yield wait_tasks()
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    als = GPS.LanguageServer.get_by_file(buf.file())
    view.goto(buf.at(4, 15).end_of_line())
    yield wait_idle()

    # Popup the completion view by adding '.'
    send_key_event(ord("e"))
    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)

    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing..."
    )
    send_key_event(ord("n"))
    yield wait_idle()
    send_key_event(GDK_RETURN)
    gps_assert(
        len([r for r in als.get_requests() if "formatting" in r.lower()]) <= 1,
        True,
        "Too many formatting requests",
    )
    yield wait_idle()
