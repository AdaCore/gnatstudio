"""
This test checks that Ada 2022 array aggregates in expression functions
don't break indentation.
"""
from gs_utils.internal.utils import *


EXPECTED = "     ['a', 'b', 'c'];"


@run_test_driver
def driver():
    b = GS.EditorBuffer.get(GS.File("main.adb"))
    b.current_view().goto(b.at(8, 1))
    GPS.execute_action("tab selection")
    line = b.get_chars(b.at(8, 1), b.at(8, 1).end_of_line())

    gps_assert(line.rstrip(), EXPECTED, "Wrong formatting in expression function")
