"""
Test that we do not preserve pending breakpoints
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.Preference("Debugger-Pending-Breakpoints").set(False)
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_tasks()

    view = Breakpoints_View()

    # create fake breakpoint for line
    ed = view.create()
    yield ed.open_and_yield()
    ed.filename.set_text('hello_c.c')
    ed.line.set_text('5')
    yield ed.ok()

    # create fake breakpoint for subprogram
    ed = view.create()
    yield ed.open_and_yield()
    combo = get_widget_by_name("Breakpoint_Type")
    combo.set_active(1)
    name = get_widget_by_name("Subprogram_Name")
    name.append_text("tmp");
    name.set_active(1)
    yield ed.ok()
    yield timeout(10000)

    # create fake breakpoint for exception
    ed = view.create()
    yield ed.open_and_yield()
    combo = get_widget_by_name("Breakpoint_Type")
    combo.set_active(3)
    name = get_widget_by_name("Exception_Name")
    name.append_text("tmp");
    name.set_active(3)
    yield ed.ok()

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    debug = GPS.Debugger.get()
    debug.start()
    yield wait_DAP_server('stackTrace')
    yield timeout(10000)

    view = Breakpoints_View()
    yield wait_idle()

    gps_assert(
        view.list.get_model().iter_n_children(), 0,
        'The breakpoint view should be empty')
