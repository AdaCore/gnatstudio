"""
This test checks that comment/uncomment work
in a project file
"""

import GPS
from gs_utils.internal.utils import *

expect = """project Test is
   for Main use ("main.adb");

   --  package Builder is
   --     for Switches ("ada") use ("-g");
   --  end Builder;

end Test;
"""


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("test.gpr"))
    orig = buf.get_chars()

    buf.current_view().goto(buf.at(4, 1))
    GPS.execute_action("comment lines")
    GPS.execute_action("comment lines")
    GPS.execute_action("comment lines")
    gps_assert(buf.get_chars(), expect, "Wrong content after commenting")

    buf.current_view().goto(buf.at(4, 1))
    GPS.execute_action("uncomment lines")
    GPS.execute_action("uncomment lines")
    GPS.execute_action("uncomment lines")
    gps_assert(buf.get_chars(), orig, "Wrong content after uncommenting")
