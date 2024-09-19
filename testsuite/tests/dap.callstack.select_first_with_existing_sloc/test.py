"""
This test checks that when the debugger gets stopped (e.g: on a breakpoint), the
Call Stack automatically selects the first frame that has an existing location, not
the first frame received from the DAP server.
"""
import GPS
from gs_utils.internal.utils import *
from gi.repository import Gdk

LOCATION_COLUMN_ID = 2
FG_COLOR_COLUMN_ID = 5


@run_test_driver
def test_driver():
    yield wait_tasks()

    # Set a breakpoint in my_print.adb
    buf = GPS.EditorBuffer.get(GPS.File("my_print.adb"))
    buf.current_view().goto(buf.at(5, 1))
    yield wait_idle()
    yield wait_until_true(
        lambda: GPS.Action("debug set line breakpoint").can_execute() == False
    )
    GPS.execute_action("debug set line breakpoint")
    yield wait_idle()

    # Launch the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield wait_for_mdi_child("Debugger Console")

    # Substitute the path for my_print.adb, so that its location
    # does not exist anymore on the disk
    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)
    debug.send("set substitute-path %s unknown.adb" % GPS.File("my_print.adb").path)
    yield timeout(1000)

    # Continue the execution until we reach the breakpoint
    debug.send("run")
    yield wait_DAP_server("stackTrace")

    # We have reached the breakpoint, but it does not refer to an existing
    # file after the path substitution: verify that the frame for my_main.adb
    # gets selected instead (frame 1, since it starts from 0).
    win = GPS.MDI.get("Call Stack").pywidget()
    tree = get_widgets_by_type(Gtk.TreeView, win)[0]
    selection = tree.get_selection()
    model, iter = selection.get_selected()
    gps_assert(
        model.get_value(iter, LOCATION_COLUMN_ID),
        GPS.File("my_main.adb").path + ":5",
        "Wrong selected frame: should be the first existing one (my_main.adb:5)",
    )
