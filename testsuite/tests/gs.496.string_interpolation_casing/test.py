"""
Verify that casing on-the-fly/end-of-line will not affect string interpolation.
"""


import GPS
from gs_utils.internal.utils import *


LINE = 8
EXPECTED = "   Put_Line (f\"The name is {Name} and the sum is {X + Y}.\");\n"


@run_test_driver
def test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    text = get_widgets_by_type(Gtk.TextView, buf.current_view().pywidget())[0]

    GPS.Preference("Ada-Casing-Policy").set("On_The_Fly")
    buf.current_view().goto(buf.at(LINE, 15))
    send_key_event(ord("\""), window=text.get_window(Gtk.TextWindowType.TEXT))
    yield timeout(1000)
    gps_assert(
        buf.get_chars(buf.at(LINE, 1), buf.at(LINE, 1).end_of_line()),
        EXPECTED,
        "Issue with auto casing On_The_Fly and string interpolation")

    GPS.Preference("Ada-Casing-Policy").set("End_Of_Line")
    buf.current_view().goto(buf.at(LINE, 1).end_of_line())
    send_key_event(GDK_RETURN, window=text.get_window(Gtk.TextWindowType.TEXT))
    yield timeout(1000)
    gps_assert(
        buf.get_chars(buf.at(LINE, 1), buf.at(LINE, 1).end_of_line()),
        EXPECTED,
        "Issue with auto casing End_Of_Line and string interpolation")
