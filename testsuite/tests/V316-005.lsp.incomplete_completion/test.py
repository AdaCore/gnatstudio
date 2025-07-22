from time import time
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    view = buf.current_view()
    view.goto(buf.at(5, 1).end_of_line())
    yield wait_tasks()

    for ch in "std::cou":
        send_key_event(ord(ch))
        yield timeout(300)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)
    pop_tree = get_widget_by_name("completion-view")
    yield wait_until_true(lambda: pop_tree.get_model() != None)

    gps_assert(
        dump_tree_model(pop_tree.get_model(), 6)[0],
        " cout",
        "Wrong first completion proposal",
    )
    GPS.execute_action("undo")

    for ch in "std::ab":
        send_key_event(ord(ch))
        yield timeout(300)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)
    pop_tree = get_widget_by_name("completion-view")
    yield wait_until_true(lambda: pop_tree.get_model() != None)

    first_proposal = dump_tree_model(pop_tree.get_model(), 6)[0]
    gps_assert(
        first_proposal,
        "•abs(…)",
        "Wrong first completion proposal",
    )
