"""
Check that contextual menu are created even with breakpoints or exception
breakpoints.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def on_gps_started():
    buf = GPS.EditorBuffer.get(GPS.File("foo.ads"))

    def check_menu(msg):
        windows = Gtk.Window.list_toplevels()
        GPS.MDI.get("foo.ads").raise_window()
        # Click on code and not empty spaces
        buf.current_view().goto(buf.at(3, 15))
        click_in_text(buf.current_view().cursor(), button=3)
        gps_assert("Jump to Implementation File" in dump_contextual(windows), True, msg)
        close_contextual(windows)

    check_menu("Without breakpoint")

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()
    debug = GPS.Debugger.get()

    main = GPS.EditorBuffer.get(GPS.File("main.adb"))
    main.current_view().goto(buf.at(5, 1))
    yield wait_idle()
    yield wait_until_not_busy(debug)
    GPS.execute_action("debug set line breakpoint")
    yield wait_for_mdi_child("Breakpoints")
    check_menu("With normal breakpoint")

    debug.send("catch exception")
    yield wait_for_mdi_child("Breakpoints")
    check_menu("With exception breakpoint")
