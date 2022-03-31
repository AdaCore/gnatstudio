"""
This test checks that we correctly hide the 'Computing...' iter
when completion list is computed and complete.
"""
from time import time
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    view = buf.current_view()
    view.goto(buf.at(5, 1).end_of_line())
    yield wait_tasks()

    for ch in "std::":
        send_key_event(ord(ch))
        yield timeout(100)

    GPS.execute_action("complete identifier (advanced)")
    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)
    yield timeout(300)

    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    gps_assert(
        model.get_value(model.get_iter_first(), 0) != "Computing...",
        True,
        "'Computing...' iter should not be shown",
    )
