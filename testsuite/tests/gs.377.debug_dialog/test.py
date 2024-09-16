"""
#377
This test checks that Yes/No dialog works with the debugger
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    debug = GPS.Debugger.get()
    debug.send("set confirm on")
    yield wait_until_not_busy(debug)
    debug.send("start")
    yield wait_until_not_busy(debug)

    yield idle_modal_dialog(lambda: GPS.execute_action("debug load file"))
    dialog = get_window_by_title("Select File to Debug")

    # select `main` executable
    tree_view = get_widgets_by_type(Gtk.TreeView, dialog)[0]
    model = tree_view.get_model()
    itr = model.get_iter_first()
    while itr != None and model.get_value(itr, 0) != "main":
        itr = model.iter_next(itr)
    tree_view.get_selection().select_iter(itr)

    ok_button = get_button_from_label("OK", dialog)
    yield idle_modal_dialog(lambda: ok_button.clicked())

    dialog = get_window_by_title("Question")
    yield idle_modal_dialog(lambda: get_button_from_label("No", dialog).clicked())
    yield wait_until_not_busy(debug)

    # Check the console's output
    console = GPS.Debugger.get().get_console()
    text = console.get_text().splitlines()
    gps_assert(text[len(text) - 1], "(gdb) ", "No prompt")
