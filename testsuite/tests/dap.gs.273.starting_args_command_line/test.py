"""
Test we don't send empty arguments to the debuggee when using run.
"""
import GPS
from gs_utils.internal.utils import *

VALUE_COLUMN = 1


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # Start the debugger with the dialog without setting any text
    debug = GPS.Debugger.get()
    debug.send("run")

    view = get_widgets_by_type(
        Gtk.TextView, GPS.MDI.get("Debugger Execution main").pywidget()
    )[0]
    buf = view.get_buffer()
    text = ""
    while not text:
        yield timeout(50)
        text = buf.get_text(buf.get_start_iter(), buf.get_end_iter(), False).strip()
    gps_assert(
        text,
        "Success",
        "The process received an unwanted argument",
    )
