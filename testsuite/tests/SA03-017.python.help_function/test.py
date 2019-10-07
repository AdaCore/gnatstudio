"""
This test checks that the 'help' function works
for GNAT Studio's Python API.
"""

import GPS
from gps_utils.internal.utils import *


doc_substring = "Splits the window in two parts"


@run_test_driver
def test():
    GPS.execute_action("open Python")
    yield wait_for_mdi_child("Python")

    win = GPS.MDI.get("Python")
    gw = win.pywidget()

    text_view = pygps.get_widgets_by_type(Gtk.TextView, gw)
    text_view = text_view[0]
    buffer = text_view.get_buffer()

    # Call 'help' on some Python API function

    text = "help(GPS.MDIWindow.split)"
    buffer.insert_interactive_at_cursor(
        text, len(text), True)
    pygps.send_key_event(pygps.GDK_RETURN, gw)

    # Get the contents of the python console and verify that
    # we retrieve some documentation.

    text = buffer.get_text(
        buffer.get_start_iter(),
        buffer.get_end_iter(),
        True)
    gps_assert(doc_substring in text, True,
               "help function not working for Python console")
