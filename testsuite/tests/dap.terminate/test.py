"""
Simple test on the terminating: verify that closing debugger console terminating the debugger.
"""
import GPS
from gs_utils.internal.utils import *
from workflows import promises


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    p = promises.DebuggerWrapper(GPS.File("main"))
    d = p.get()
    yield wait_until_not_busy(d)

    yield idle_modal_dialog(lambda: GPS.execute_action("debug continue"))
    dialog = get_window_by_title("Run/Start")
    get_widgets_by_type(Gtk.CheckButton, dialog)[0].set_active(False)
    get_button_from_label("OK", dialog).clicked()

    yield wait_for_mdi_child("Debugger Console main" + dot_exe)
    console = GPS.MDI.get("Debugger Console main" + dot_exe)
    console.close()
    yield hook("DAP_debugger_unloaded")
    yield wait_idle()

    gps_assert(GPS.MDI.current_perspective(), "Default", "Incorrect perspective")
