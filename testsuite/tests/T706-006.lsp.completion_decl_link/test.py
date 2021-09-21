"""
Verify that a link to the completion proposals' declaration is displayed
at the top of the completion notes window when selecting an item in the
completion window.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(4, 9))
    yield wait_idle()

    for ch in "ria":
        send_key_event(ord(ch))
        yield timeout(100)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)
    pop_tree = get_widget_by_name("completion-view")
    yield wait_until_true(lambda: pop_tree.get_model().get_iter_first() != None)
    model = pop_tree.get_model()
    pop_tree.get_selection().select_iter(model.get_iter_first())
    yield wait_language_server("completionItem/resolve")

    title_box = get_widget_by_name("completion-notes-title")
    link_button = get_widgets_by_type(Gtk.LinkButton, title_box)[0]

    gps_assert(link_button != None, True,
               "The completion proposal's declaration link is not displayed")

    gps_assert(link_button.get_uri(), "main.adb:2",
               "The completion proposal's declaration link is not correct")
