"""
Check the trigger character for Ada (at least '(' and ',')
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

    # Close the window and write some characters
    send_key_event(GDK_ESCAPE, window=main_window)
    yield wait_idle()
    gps_assert(
        get_widget_by_name("signature-help-window"),
        None,
        "Escape should close the window",
    )
    for c in "1234 + 4567":
        send_key_event(ord(c), window=main_window)
        yield timeout(10)
    gps_assert(
        get_widget_by_name("signature-help-window"),
        None,
        "The window should still be closed",
    )

    # Use the second trigger character and check the highlighting
    send_key_event(ord(","), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")
    send_key_event(ord(" "), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")
    send_key_event(ord("2"), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")
    signature_help_window = get_widget_by_name("signature-help-window")
    labels = get_widgets_by_type(Gtk.Label, signature_help_window)
    gps_assert(labels[1].get_label(), SECOND_HIGH, "Wrong highlighting")
