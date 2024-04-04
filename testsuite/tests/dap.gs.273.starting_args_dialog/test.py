"""
Test we don't send empty arguments to the debuggee when pressing Run/Start.
"""
import GPS
from gs_utils.internal.utils import *

VALUE_COLUMN = 1


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # Start the debugger with the dialog without setting any text
    yield idle_modal_dialog(lambda: GPS.execute_action("debug continue"))
    dialog = get_window_by_title("Run/Start")
    entry = get_widgets_by_type(Gtk.Entry, dialog)[0]
    ok_button = get_button_from_label("OK", dialog)
    ok_button.clicked()
    yield hook("debugger_location_changed")

    GPS.execute_action("debug continue")
    view = get_widgets_by_type(
        Gtk.TextView, GPS.MDI.get("Debugger Execution main").pywidget()
    )[0]
    buf = view.get_buffer()
    text = ""
    while not text:
        yield timeout(50)
        text = buf.get_text(buf.get_start_iter(),
                            buf.get_end_iter(), False).strip()
    gps_assert(
        text,
        "Success",
        "The process received an unwanted argument",
    )
