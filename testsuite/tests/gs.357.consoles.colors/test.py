"""
This test verifies that color preferences are correctly
applied to GNAT Studio consoles, depending on the type
of messages that have been output (e.g: errors).
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs
import re


def check_highlighting_color(buffer, line, color):
    start = buffer.get_iter_at_line(line)
    end = start.copy()
    end.forward_to_line_end()
    format = buffer.register_serialize_tagset()
    serialized_text = str(buffer.serialize(buffer, format, start, end))
    gps_assert(color in serialized_text, True, "Wrong fg color for line %s" % str(line))

@run_test_driver
def run_test():
    # Open a custom console and write an error in it
    GPS.Console("Testing").write("This is an error", mode="error")
    console = MDI.get("Testing").get_child().pywidget()
    buffer = get_widgets_by_type(Gtk.TextView, console)[0].get_buffer()

    # Verify that the error message is highlighted with the default color
    check_highlighting_color(buffer, 0, "c8c8:2a2a:2a2a")

    # Change the error message's color preference, and check that the new
    # color gets applied to the error message
    GPS.Preference("Messages-Highlight-Color").set("rgb(165,29,45)")
    yield timeout(300)
    check_highlighting_color(buffer, 0, "a5a5:1d1d:2d2d")

