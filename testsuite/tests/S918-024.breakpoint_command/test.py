"""
Verify that GNAT Studio can set commands on breakpoint.

"""
import GPS
from gs_utils.internal.utils import *
import workflows
from workflows import promises


@run_test_driver
def test_driver():
    mode = "Mode:" + GPS.Preference("GPS6-Debugger-Debugger-Kind").get()
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    p = promises.DebuggerWrapper(GPS.File("main"))
    d = p.get()
    yield wait_until_not_busy(d)
    yield p.send_promise("b main.adb:31")
    yield wait_until_not_busy(d)

    view = Breakpoints_View()
    yield view.select(0)
    ed = view.edit()  # open breakpoint editor
    yield ed.open_and_yield()

    ed.command_text_view.get_buffer().set_text("cont")

    yield ed.ok()

    yield wait_idle()
    info = yield p.send_promise("info breakpoints")
    gps_assert(
        info.data.find("cont") != -1,
        True,
        mode + " Breakpoint command has not been set ",
    )

    d.send("q")
    yield wait_tasks()
