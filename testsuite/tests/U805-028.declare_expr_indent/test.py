"""
This test checks that Ada 2022 declare expressions don't break
indentation in GS.
"""
from gs_utils.internal.utils import *


EXPECTED = "   function Foo(x : Integer) return Integer is (x);"


@run_test_driver
def driver():
    b = GS.EditorBuffer.get(GS.File("main.adb"))
    b.current_view().goto(b.at(7, 1))
    GPS.execute_action("tab selection")
    line = b.get_chars(b.at(7, 1), b.at(7, 1).end_of_line())

    gps_assert(line.rstrip(),
               EXPECTED, "Wrong formatting after declare expression")

    GPS.execute_action("undo")
