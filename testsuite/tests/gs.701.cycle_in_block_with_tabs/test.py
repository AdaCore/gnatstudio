"""
Test the action "cycle in block" with a file containing tabs
"""

from GPS import *
from gs_utils.internal.utils import *

CYCLE = "cycle in block"


def verify(buf, init, locs, msg, col=1):
    buf.current_view().goto(buf.at(init, col))
    step = 0
    for line, col in locs:
        GPS.execute_action(CYCLE)
        step = step + 1
        cursor = buf.current_view().cursor()
        gps_assert(cursor.line(), line, msg + " | wrong line for step: " + str(step))
        gps_assert(cursor.column(), col, msg + " | wrong col for step: " + str(step))


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    verify(buf, 2, [(12, 1), (18, 1), (1, 1), (12, 1)], "cycling in main")
    verify(buf, 14, [(15, 4), (17, 4), (13, 4), (15, 4)], "cycling in declare")
    verify(buf, 16, [(17, 4), (13, 4), (15, 4)], "cycling in declare v2")
    verify(buf, 8, [(9, 4), (11, 4), (8, 4), (9, 4)], "cycling in nested proc", col=4)

    buf = GPS.EditorBuffer.get(GPS.File("bar.adb"))
    verify(buf, 7, [(11, 4), (6, 4), (7, 4), (11, 4)], "cycling in task")
    verify(buf, 21, [(22, 7), (24, 7), (20, 7), (22, 7)], "cycling in entry")
