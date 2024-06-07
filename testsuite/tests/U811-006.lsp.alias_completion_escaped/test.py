"""
This test checks that we correctly escape alias completion items
documentation.
"""
import GPS
from gs_utils.internal.utils import *


LABEL_COLUMN = 6


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_tasks()

    # Trigger the completion by typing 'hd'.
    # We want to have the 'hdr' alias in the results, since it's doc
    # contains special chars that need to be escaped.
    for ch in "hd":
        send_key_event(ord(ch))
        yield timeout(100)

    # Wait for the completion window...
    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)
    pop_tree = get_widget_by_name("completion-view")

    # Verify that we have 'hdr (alias)' listed
    gps_assert(
        "hdr (alias)" in dump_tree_model(pop_tree.get_model(), LABEL_COLUMN),
        True,
        "'hdr' alias should be present in completion results",
    )

    # Navigate among completion results: no Gtk warning should appear
    send_key_event(GDK_DOWN)
    send_key_event(GDK_DOWN)
    yield wait_idle()
