
"""
This test checks that the signature help works on a parameterless C function
"""

import GPS
from gs_utils.internal.utils import *

EXPECTED = "getchar() -> int"

@run_test_driver
def run_test():
    # Open main.c
    buf = GPS.EditorBuffer.get(GPS.File("main.c"))
    yield wait_tasks()

    view = buf.current_view()
    yield wait_tasks(other_than=known_tasks)
    view.goto(buf.at(4, 12))

    main_window = GPS.MDI.get_main_window().pywidget()

    # Trigger the signatureHelp request by typing '('
    send_key_event(ord('('), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "C")

    signature_help_window = get_widget_by_name("signature-help-window")
    labels = get_widgets_by_type(Gtk.Label, signature_help_window)

    # Verify that all the label contents are correct
    gps_assert(labels[1].get_text(), EXPECTED,
               "The active signature label text is not correct")
