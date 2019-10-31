"""
This test verifies that in multicursor mode all the cursors are correctly
moved.
"""

from GPS import *
from gs_utils.internal.utils import *


def check_location(buf, locs, msg):
    jx = 0
    for cursor in buf.cursors():
        mark = cursor.mark()
        gps_assert((mark.line, mark.column),
                   locs[jx],
                   "Wrong cursor loc " + msg)
        jx += 1


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    GPS.execute_action("Add cursor and go down")
    check_location(buf, [(2, 1), (1, 1)], "after creating the second cursor")
    GPS.execute_action("Move to next char")
    check_location(buf, [(2, 2), (1, 2)], "after next char")
    GPS.execute_action("Move to next line")
    check_location(buf, [(3, 2), (2, 2)], "after next line")
    GPS.execute_action("Move to next line")
    check_location(buf, [(4, 2), (3, 2)], "after next line")
