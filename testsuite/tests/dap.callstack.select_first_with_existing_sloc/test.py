"""
This test checks that when the debugger gets stopped (e.g: on a breakpoint), the
Call Stack automatically selects the first frame that has an existing location, not
the first frame received from the DAP server.
"""

import GPS
from gs_utils.internal.utils import *
from gi.repository import Gdk

LOCATION_COLUMN_ID = 2


@run_test_driver
def test_driver():
    yield wait_tasks()

    # Launch the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield wait_for_mdi_child("Debugger Console")

    # Set a breakpoint in fwrite - this will be called by the call to Put_Line
    # in my_print.adb
    debug = GPS.Debugger.get()
    debug.send("break fwrite")
    yield wait_DAP_server("setBreakpoints")
    yield wait_idle()

    # Send a 'start' command to load the symbols table, making the DAP
    # GDB server to recognize the breakpoint on the 'frwite' function.
    debug.send("start")
    yield wait_DAP_server("stackTrace")
    yield wait_idle()

    # Continue the execution until we reach the breakpoint
    debug.send("run")
    yield wait_DAP_server("stackTrace")
    yield wait_idle()

    # We have reached the breakpoint, but it does not refer to an existing
    # file in the disk: verify that the frame for my_print.adb gets selected.
    win = GPS.MDI.get("Call Stack").pywidget()
    tree = get_widgets_by_type(Gtk.TreeView, win)[0]
    selection = tree.get_selection()
    model, iter = selection.get_selected()
    row_value = model.get_value(iter, LOCATION_COLUMN_ID)
    gps_assert(
        row_value.endswith("my_print.adb:5"),
        True,
        "Wrong selected frame: should be the first "
        + "existing one (my_print.adb:5). Instead we got: %s" % row_value,
    )
