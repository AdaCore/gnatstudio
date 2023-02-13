"""
Checks whether we show the location of the selected task
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    d = GPS.Debugger.get()
    for s in ["b main.adb:22",
              "run"]:
        yield wait_until_not_busy(d)
        d.send(s)

    yield wait_until_not_busy(d)
    GPS.execute_action("open tasks debugger window")

    d.send("task 2")
    yield wait_idle()

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    gps_assert(b.current_view().cursor().line() == 22, False,
               "should be located in the task source")

    d.send("task 1")
    yield wait_idle()

    gps_assert(b.current_view().cursor().line(), 22,
               "should be located in the main")
    d.send('q')

    yield wait_tasks()
