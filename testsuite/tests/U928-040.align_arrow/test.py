"""
Tests whether Align->Arrow symbols works as expected
"""

import GPS
from gs_utils.internal.utils import *

FILE = "align.ads"
expected = """     (Key_Type        => Integer,
      Element_Type    => Integer,
      Hash            => Hash,
      Equivalent_Keys => "=");
"""


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File(FILE))
    buf.main_cursor().move(buf.at(10, 29))
    buf.select(buf.at(7, 7), buf.at(10, 29))
    GPS.execute_action("Align arrows")
    gps_assert(GPS.Editor.get_chars(FILE), expected, "Not formatted as expected")
