"""
Test the 'debug start' dialog, which allows to pass arguments
to the debuggee launched by the DAP server.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    # Disable the Debugger Execution window to get the debuggee's output
    # directly in the Messages view
    yield wait_tasks()

    # Launch the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # start the debugger: it should spawn the dialog
    yield idle_modal_dialog(lambda: GPS.execute_action("debug continue"))
    dialog = get_window_by_title("Run/Start")
    entry = get_widgets_by_type(Gtk.Entry, dialog)[0]
    ok_button = get_button_from_label("OK", dialog)
    entry.set_text("-i 10")
    ok_button.clicked()
    yield hook("debugger_location_changed")

    # Continue the execution
    GPS.execute_action("debug continue")
    yield timeout(1000)

    # Check the Debugger Execution view's contents
    # Check that the debuggee's output is displayed in the execution console
    view = get_widgets_by_type(
        Gtk.TextView, GPS.MDI.get("Debugger Execution main").pywidget()
    )[0]
    buf = view.get_buffer()
    gps_assert(
        buf.get_text(buf.get_start_iter(), buf.get_end_iter(), False).strip(),
        "Value is: 10",
        "The specified argument has not been passed to the debuggee",
    )
