"""
Test the signatureHelp active parameter highlighting for complex naming
case.
"""

import GPS
from gs_utils.internal.utils import *

FIRST_HIGH = "procedure Foo_Bar_Foo2 (<b>Foo2</b>: My_Int; Foo : My_Int)"
SECOND_HIGH = "procedure Foo_Bar_Foo2 (Foo2: My_Int; <b>Foo</b> : My_Int)"


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    view = buf.current_view()
    yield wait_tasks(other_than=known_tasks)
    view.goto(buf.at(9, 16).end_of_line())
    yield wait_idle()

    main_window = GPS.MDI.get_main_window().pywidget()

    # Trigger the signatureHelp request by typing '('
    send_key_event(ord("("), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")

    signature_help_window = get_widget_by_name("signature-help-window")
    labels = get_widgets_by_type(Gtk.Label, signature_help_window)

    gps_assert(labels[1].get_label(), FIRST_HIGH, "Wrong highlighting")

    for c in "1, ":
        send_key_event(ord(c), window=main_window)
        yield timeout(10)
    send_key_event(ord("2"))
    yield wait_language_server("textDocument/signatureHelp", "Ada")
    gps_assert(labels[1].get_label(), SECOND_HIGH, "Wrong highlighting")
