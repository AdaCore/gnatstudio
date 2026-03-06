"""
Verify that on-the-fly/end-of-word casing does not affect words inside
string literals when the closing quote is typed.
"""

import GPS
from gs_utils.internal.utils import *


LINE = 2
EXPECTED = '   X : constant String := "hello_world"\n'


@run_test_driver
def test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    text = get_widgets_by_type(Gtk.TextView, buf.current_view().pywidget())[0]

    GPS.Preference("Ada-Casing-Policy").set("On_The_Fly")
    # Position cursor at end of the incomplete string (after "hello_world)
    buf.current_view().goto(buf.at(LINE, 1).end_of_line())
    send_key_event(ord('"'), window=text.get_window(Gtk.TextWindowType.TEXT))
    yield timeout(1000)
    gps_assert(
        buf.get_chars(buf.at(LINE, 1), buf.at(LINE, 1).end_of_line()),
        EXPECTED,
        "Casing should not be applied inside string literals",
    )
