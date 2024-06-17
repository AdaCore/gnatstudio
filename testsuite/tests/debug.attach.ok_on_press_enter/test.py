"""
This test checks that pressing Enter while focusing the 'debug attach'
dialog correctly activates the dialog's default response (which is 'OK').
"""

import GPS
import os
from gs_utils.internal.utils import *

ATTACH_DIALOG_NAME = "Select the process to attach to"


@run_test_driver
def test_driver():
    # Build the project
    GPS.execute_action("Build All")
    yield wait_tasks()

    # Run the process
    proc = GPS.Process("./main_t819_019")
    pid = proc.get_pid()

    # Initialize a debug session without any main
    GPS.execute_action("/Debug/Initialize/no main file")
    yield wait_idle()
    debug = GPS.Debugger.get()

    # Attach to the running 'main' process
    yield idle_modal_dialog(lambda: GPS.execute_action("debug attach"))
    dialog = get_window_by_title(ATTACH_DIALOG_NAME)
    entry = get_widgets_by_type(Gtk.Entry, dialog)[0]
    ok_button = get_button_from_label("OK", dialog)

    # Press the 'Enter' key and check that we have attached to
    # the running process.
    entry.set_text(str(pid))
    send_key_event(GDK_RETURN)
    yield timeout(1000)

    gps_assert(
        len(debug.frames()) > 0,
        True,
        "We should have frames after attaching to the process",
    )

    # Detach the debugger
    GPS.execute_action("debug detach")

    # Kill the process manually
    proc.kill()
