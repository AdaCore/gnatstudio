"""
This test checks that pressing Enter while focusing the 'debug attach'
dialog correctly activates the dialog's default response (which is 'OK').
The list of processes has been filled using psutil.
"""

import GPS
import os
from gs_utils.internal.utils import *

ATTACH_DIALOG_NAME = "Select the process to attach to"


@run_test_driver
def test_driver():
    # Enable using psutils to list processes
    GPS.Preference("Helpers-Py-List-Processes").set(True)
    # Build the project
    GPS.execute_action("Build All")
    yield wait_tasks()

    # Initialize a debug session without any main
    GPS.execute_action("/Debug/Initialize/no main file")
    yield wait_idle()
    debug = GPS.Debugger.get()

    # Run the process
    proc = GPS.Process("./main_t819_019")

    # Open the attach dialog
    yield idle_modal_dialog(lambda: GPS.execute_action("debug attach"))
    dialog = get_window_by_title(ATTACH_DIALOG_NAME)
    entry = get_widgets_by_type(Gtk.Entry, dialog)[0]
    tree = get_widgets_by_type(Gtk.TreeView, dialog)[0]
    ok_button = get_button_from_label("OK", dialog)

    # Find the process pid using its name
    pid = None
    data = dump_tree_model(tree.get_model())
    for d in data:
        if "main_t819_019" in d[1]:
            pid = d[0]
            break

    gps_assert(pid is not None, True, "Could not find the pid")
    entry.set_text(str(pid))
    ok_button.clicked()
    yield hook("debugger_location_changed")
    yield wait_idle()

    gps_assert(
        len(debug.frames()) > 0,
        True,
        "We should have frames after attaching to the process",
    )

    # Detach the debugger
    GPS.execute_action("debug detach")

    # Kill the process manually
    proc.kill()
