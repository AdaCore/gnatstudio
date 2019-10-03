"""
Verify that GNAT Studio can set commands on breakpoint.

"""
import GPS
from gps_utils.internal.utils import *


@run_test_driver
def test_driver():
    mode = "Mode:" + GPS.Preference("GPS6-Debugger-Debugger-Kind").get() + "\n"
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    d = GPS.Debugger.get()
    d.send("b main.adb:31")
    yield wait_until_not_busy(d)

    view = Breakpoints_View()
    yield view.select(0)
    ed = view.edit()    # open breakpoint editor
    yield ed.open_and_yield()

    ed.command_text_view.get_buffer().set_text('cont')

    yield ed.ok()

    yield wait_idle()
    info = d.send("info breakpoints")
    gps_assert(info.find("cont") != -1, True, mode +
               "Breakpoint command has not been set")

    d.send('q')
    yield wait_tasks()
