"""
This test checks that "aggregate" is considered as keyword and thus
gets lower cased when reformatting qbug.gpr.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Ada-Casing-Policy").set("End_Of_Line")
    buf = GPS.EditorBuffer.get(GPS.File("qbug.gpr"))
    buf.select()
    GPS.execute_action("format selection")
    gps_assert(buf.get_chars(buf.at(1, 1), buf.at(1, 1).end_of_line()),
               "aggregate project Qbug is\n",
               "Wrong indentation")
