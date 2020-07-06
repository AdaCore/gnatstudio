"""
Verify that a link to the completion proposals' declaration is displayed
at the top of the completion notes window when selecting an item in the
completion window.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(4, 9))
    yield wait_idle()

    # Insert a completion snippet received from clangd
    for ch in "ria":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    click_in_tree(pop_tree, path="0")

    title_box = get_widget_by_name("completion-notes-title")
    link_button = get_widgets_by_type(Gtk.LinkButton, title_box)[0]

    gps_assert(link_button != None, True,
               "The completion proposal's declaration link is not displayed")

    gps_assert(link_button.get_uri(), "main.adb:2",
               "The completion proposal's declaration link is not correct")
