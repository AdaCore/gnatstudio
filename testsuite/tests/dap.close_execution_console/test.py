"""
Check that closing the execution console terminates the debugger.
"""
import GPS
from gs_utils.internal.utils import *
import re
import workflows
from workflows import promises


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield wait_for_mdi_child("Debugger Execution main")
    yield wait_idle()

    yield idle_modal_dialog(lambda: GPS.execute_action("debug continue"))
    dialog = get_window_by_title("Run/Start")
    get_widgets_by_type(Gtk.CheckButton, dialog)[0].set_active(False)
    get_button_from_label("OK", dialog).clicked()
    yield timeout(100)

    console = GPS.MDI.get("Debugger Execution main")
    console.close()

    yield hook("DAP_debugger_unloaded")
    yield wait_idle()

    gps_assert(GPS.MDI.current_perspective(), "Default", "Incorrect perspective")
