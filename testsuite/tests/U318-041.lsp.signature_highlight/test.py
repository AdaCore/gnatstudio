"""
Verify that we highlight only the matching parameter.
When we have a signature with 2 parameters AA and AAAA when AA is active
AAAA should not be highlighted.
"""

import GPS
from gs_utils.internal.utils import *

FIRST_HIGH = "procedure Do_Something (<b>AA</b> : Integer; AAAA : Integer)"
SECOND_HIGH = "procedure Do_Something (AA : Integer; <b>AAAA</b> : Integer)"


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    yield wait_tasks(other_than=known_tasks)
    view.goto(buf.at(14, 17).end_of_line())
    yield wait_idle()

    main_window = GPS.MDI.get_main_window().pywidget()

    # Trigger the signatureHelp request by typing '('
    send_key_event(ord("("), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")

    signature_help_window = get_widget_by_name("signature-help-window")
    labels = get_widgets_by_type(Gtk.Label, signature_help_window)

    # Basic starting case: first parameter should be highlighted
    gps_assert(labels[1].get_label(), FIRST_HIGH, "Wrong highlighting")

    # Name parameter should be recognized regarding of the order
    for c in "AAAA => ":
        send_key_event(ord(c), window=main_window)
        yield timeout(10)
    send_key_event(ord("1"), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")
    gps_assert(labels[1].get_label(), SECOND_HIGH, "Wrong highlighting")
