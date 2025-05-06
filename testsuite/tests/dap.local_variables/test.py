"""
Verify that the Variables view can properly compute simple type.
"""

from gs_utils.internal.utils import *
from workflows import promises

expect = ['<b>Local variables</b> =  ',
  ['<b>foo</b> = array (1 .. 12) of character &quot;Hello World!&quot;',
  '<b>bar</b> = array (1 .. 10) of character &quot;aaaaaaaaaa&quot;',
  '<b>i</b> = integer 1']]


@run_test_driver
def driver():
    yield wait_tasks()
    GPS.execute_action("Build & Debug Number 1")
    yield wait_for_mdi_child("Debugger Console")

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))

    p = promises.DebuggerWrapper(GPS.File("foo"))
    debug = p.get()
    yield wait_until_not_busy(debug)

    yield p.send_promise("break main.adb:13")
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

    d = view.dump()
    gps_assert(d is not None, True, "Variables view is empty")
    gps_assert(expect, d, "Invalid contents of the Variables view")
