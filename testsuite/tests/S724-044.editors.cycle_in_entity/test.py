"""
Test the action "cycle in block"
"""

from GPS import *
from gps_utils.internal.utils import *

CYCLE = "cycle in block"


def verify(buf, init, l, msg, col=1):
    buf.current_view().goto(buf.at(init, col))
    step = 0
    for line in l:
        GPS.execute_action(CYCLE)
        step = step + 1
        gps_assert(buf.current_view().cursor().line(),
                   line,
                   msg + " | step: " + str(step))


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    verify(buf, 2, [12, 18, 1, 12], "cycling in main")
    verify(buf, 14, [15, 17, 13, 15], "cycling in declare")
    verify(buf, 16, [17, 13, 15], "cycling in declare v2")
    verify(buf, 8, [9, 11, 8, 9], "cycling in nested proc", col=4)

    buf = GPS.EditorBuffer.get(GPS.File("bar.adb"))
    verify(buf, 7, [11, 6, 7, 11], "cycling in task")
    verify(buf, 21, [22, 24, 20, 22], "cycling in entry")
