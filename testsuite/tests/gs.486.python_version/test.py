"""
Verify the version of python embedded inside GS.
"""

import GPS
from gs_utils.internal.utils import *


version = "3.13"


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

    text = "sys.version"
    buffer.insert_interactive_at_cursor(text, len(text), True)
    pygps.send_key_event(pygps.GDK_RETURN, gw)
    yield wait_idle()

    # Get the contents of the python console and verify that
    # we retrieve some documentation.

    text = buffer.get_text(buffer.get_start_iter(), buffer.get_end_iter(), True)
    gps_assert(
        version in text,
        True,
        "Wrong version of python",
    )
