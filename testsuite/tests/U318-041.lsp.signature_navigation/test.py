
"""
Test the navigation inside the signatureHelp window using Up/Down.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    yield wait_tasks(other_than=known_tasks)
    view.goto(buf.at(34, 17).end_of_line())
    yield wait_idle()

    main_window = GPS.MDI.get_main_window().pywidget()

    # Trigger the signatureHelp request by typing '('
    send_key_event(ord('('), window=main_window)
    yield wait_language_server("textDocument/signatureHelp", "Ada")

    signature_help_window = get_widget_by_name("signature-help-window")
    labels = get_widgets_by_type(Gtk.Label, signature_help_window)

    # Down (Up) should select the next (previous) signature if any and
    # close the window when at the edge.
    gps_assert(labels[0].get_text(), "1/3",
               "The selector label text is not correct")
    send_key_event(Gdk.KEY_Down, window=main_window)
    gps_assert(labels[0].get_text(), "2/3",
               "The selector label text is not correct")
    send_key_event(Gdk.KEY_Up, window=main_window)
    gps_assert(labels[0].get_text(), "1/3",
               "The selector label text is not correct")
    send_key_event(Gdk.KEY_Down, window=main_window)
    send_key_event(Gdk.KEY_Down, window=main_window)
    gps_assert(labels[0].get_text(), "3/3",
               "The selector label text is not correct")
    send_key_event(Gdk.KEY_Down, window=main_window)
    gps_assert(get_widget_by_name("signature-help-window"), None,
               "The window should be closed")
