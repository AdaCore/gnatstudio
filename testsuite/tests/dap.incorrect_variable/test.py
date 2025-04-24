"""
Verify that the Variables view shows error message as a value
when GDB can't get the value.
"""

from gs_utils.internal.utils import *
from workflows import promises


@run_test_driver
def driver():
    yield wait_tasks()
    GPS.execute_action("Build & Debug Number 1")
    yield wait_for_mdi_child("Debugger Console")

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))

    p = promises.DebuggerWrapper(GPS.File("main"))
    debug = p.get()
    yield wait_until_not_busy(debug)

    yield p.send_promise("break main.adb:18")
    yield wait_until_not_busy(debug)

    yield p.send_promise("run")
    yield wait_DAP_server('stackTrace')
    yield wait_idle()

    GPS.MDI.get_by_child(b.current_view()).raise_window()
    b.current_view().goto(b.at(14, 4))

    select_editor_contextual("Debug/Display I in Variables view")
    yield wait_until_not_busy(debug)

    view = Variables_View()
    yield view.open_and_yield()

    view.expand([0])
    yield wait_until_not_busy(debug)
    
    gps_assert(
        "Cannot access memory" in view.dump()[1][0],
        True,
        "Invalid contents of the Variables view")
