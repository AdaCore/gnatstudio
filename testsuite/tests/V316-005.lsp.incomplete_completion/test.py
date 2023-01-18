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
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    gps_assert(
        dump_tree_model(pop_tree.get_model(), 6)[0],
        " cout",
        "Wrong first completion proposal",
    )
    GPS.execute_action("undo")

    for ch in "std::fa":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    gps_assert(
        dump_tree_model(pop_tree.get_model(), 6)[0],
        "•false_type",
        "Wrong first completion proposal",
    )
