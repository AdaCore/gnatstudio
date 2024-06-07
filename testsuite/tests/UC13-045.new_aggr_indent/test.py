"""
This test checks that Ada 2022 aggregates don't break indentation.
"""
from gs_utils.internal.utils import *


EXPECTED = "      others => 'a'];"


@run_test_driver
def driver():
    b = GS.EditorBuffer.get(GS.File("main.adb"))
    b.current_view().goto(b.at(4, 1))
    GPS.execute_action("tab selection")
    line = b.get_chars(b.at(4, 1), b.at(4, 1).end_of_line())

    gps_assert(line.rstrip(), EXPECTED, "Wrong formatting after declare expression")
