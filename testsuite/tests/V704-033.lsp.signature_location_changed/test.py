
"""
Test the navigation inside the signatureHelp window using Left/Right arrow.
"""

import GPS
from gs_utils.internal.utils import *

FIRST_PARAM = "procedure Do_Something (<b>A</b> : Integer; B : Integer)"
SECOND_PARAM = "procedure Do_Something (A : Integer; <b>B</b> : Integer)"


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    yield wait_tasks(other_than=known_tasks)
    view.goto(buf.at(5, 1).end_of_line())
    yield wait_idle()

    main_window = GPS.MDI.get_main_window().pywidget()

    # Trigger the signatureHelp request by typing ','
    send_key_event(ord(','), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")

    signature_help_window = get_widget_by_name("signature-help-window")
    labels = get_widgets_by_type(Gtk.Label, signature_help_window)
    gps_assert(labels[1].get_label(), SECOND_PARAM,
               "issue when opening the signatureHelp")

    send_key_event(GDK_LEFT, window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")
    send_key_event(GDK_LEFT, window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")
    gps_assert(labels[1].get_label(), FIRST_PARAM,
               "signatureHelp has not been updated for the location changed")
