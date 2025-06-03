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

    # select `test` executable
    tree_view = get_widgets_by_type(Gtk.TreeView, dialog)[0]
    yield timeout(100)
    model = tree_view.get_model()
    itr = model.get_iter_first()
    while itr != None and model.get_value(itr, 0) != "test":
        itr = model.iter_next(itr)
    tree_view.get_selection().select_iter(itr)

    ok_button = get_button_from_label("OK", dialog)
    yield idle_modal_dialog(lambda: ok_button.clicked())

    yield wait_DAP_server("stackTrace")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    gps_assert(debug.current_file.base_name(), "test.adb", "Wrong current file")
