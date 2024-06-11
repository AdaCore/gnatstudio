"""
Verify that load file action works over DAP
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("debug initialize Default:no main file")
    yield hook("debugger_started")
    yield wait_idle()

    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)
    yield wait_idle()

    yield idle_modal_dialog(lambda: GPS.execute_action("debug load file"))
    dialog = get_window_by_title("Select File to Debug")

    tree_view = get_widgets_by_type(Gtk.TreeView, dialog)[0]
    tree_view.get_selection().select_path("7")

    ok_button = get_button_from_label("OK", dialog)
    yield idle_modal_dialog(lambda: ok_button.clicked())

    yield wait_until_not_busy(debug)
    yield wait_idle()

    yield wait_until_true(
        lambda: GPS.EditorBuffer.get(GPS.File("test.adb"), open=False) is None
    )
    gps_assert(
        GPS.EditorBuffer.get(GPS.File("test.adb"), open=False) is not None,
        True,
        "Invalid file opened",
    )
