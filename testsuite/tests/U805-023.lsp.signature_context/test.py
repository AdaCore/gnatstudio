
"""
- Open the signature window help
- Select the second signature
- Adding characters should keep the second signature selected
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    yield wait_tasks(other_than=known_tasks)
    view.goto(buf.at(25, 1).end_of_line())
    yield wait_idle()

    main_window = GPS.MDI.get_main_window().pywidget()

    # Trigger the signatureHelp request by typing '('
    send_key_event(ord('('), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")

    signature_help_window = get_widget_by_name("signature-help-window")
    labels = get_widgets_by_type(Gtk.Label, signature_help_window)
    gps_assert(labels[0].get_text(),
               "1/2",
               "Issue when opening the view")
    send_key_event(GDK_DOWN, window=main_window)
    yield wait_idle()
    gps_assert(labels[0].get_text(),
               "2/2",
               "Issue after down")
    for c in "1 + 2":
        send_key_event(ord(c), window=main_window)
        yield wait_language_server("textDocument/signatureHelp", "Ada")
    gps_assert(labels[0].get_text(),
               "2/2",
               "Issue after adding characters")
    send_key_event(ord(","), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")
    gps_assert(labels[0].get_text(),
               "1/1",
               "Issue after starting to write the next parameters")
