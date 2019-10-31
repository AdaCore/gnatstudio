"""
Functions retrieving the buffer content should not return the special lines
"""

import GPS
from gs_utils.internal.utils import *

FILE = "main.adb"


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File(FILE))
    expected = buf.get_chars()
    buf.add_special_line(1, "-- :)")
    buf.add_special_line(3, "-- :P")
    buf.add_special_line(5, "-- ;)")
    # Needed by GPS.Editor.get_chars
    buf.select()
    gps_assert(GPS.Editor.get_chars(FILE),
               expected,
               "GPS.Editor.get_chars issue")
    gps_assert(buf.get_chars(),
               expected,
               "GPS.EditorBuffer.get_chars issue")
    gps_assert(GPS.Editor.get_buffer(FILE),
               expected,
               "GPS.Editor.get_buffer issue")
