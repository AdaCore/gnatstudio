"""
Verify that the variable view can display methods' arguments
"""
import GPS
from gs_utils.internal.utils import *

data = ['<b>x</b> = integer 2']


@run_test_driver
def test_driver():
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)
    yield debug.send("b main.adb:6")
    yield wait_until_not_busy(debug)

    debug.send("run")
    yield wait_until_not_busy(debug)

    view = Variables_View()
    yield view.open_and_yield()

    Variables_View.display("X")
    yield wait_DAP_server("variables")
    yield wait_idle()

    gps_assert(
        data,
        view.dump(),
        "Invalid contents in the Variables view")    
