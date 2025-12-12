"""
Verify that the Non ASCII variables names displayed in the variables view
"""

import GPS
from gs_utils.internal.utils import *
from workflows import promises


expect = [
    "<b>Local variables</b> =  ",
    [
        "<b>is_bool</b> = boolean false",
        "<b>ЗМІННА</b> = universal_real 3.1400000000000000001",
    ],
]


@run_test_driver
def test_driver():
    yield wait_tasks()
    GPS.execute_action("Build & Debug Number 1")
    yield wait_for_mdi_child("Debugger Console")

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    p = promises.DebuggerWrapper(GPS.File("main"))
    debug = p.get()
    yield wait_until_not_busy(debug)

    yield p.send_promise("break main.adb:10")
    yield wait_until_not_busy(debug)

    yield p.send_promise("run")
    yield hook("debugger_location_changed")
    yield wait_idle()

    view = Variables_View()
    yield view.open_and_yield()
    yield wait_idle()

    GPS.execute_action("debug tree display local variables")
    yield wait_DAP_server("variables")
    yield wait_idle()

    gps_assert(view.dump(), expect, "Incorrect variables")
