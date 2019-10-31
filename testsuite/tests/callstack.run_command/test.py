"""
Simple test on the callstack: verify that the view is correctly updated
when running and stopping on a breakpoint.
"""
import GPS
from gs_utils.internal.utils import *

NAME_COLUMN = 2
LOCATION_COLUMN = 4


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(5, 1))
    GPS.execute_action("debug set line breakpoint")

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    debug = GPS.Debugger.get()

    GPS.execute_action("open debugger call stack")
    yield wait_for_mdi_child("Call Stack")
    view = GPS.MDI.get("Call Stack")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    # The view should be empty by default
    gps_assert(dump_tree_model(model, NAME_COLUMN),
               [],
               "Wrong content when opening the callstack")

    debug.send("run")
    yield wait_until_not_busy(debug)
    # Verify the view was correctly updated by the run/break command
    gps_assert(dump_tree_model(model, NAME_COLUMN),
               ["main"],
               "Wrong content after breaking")
    # Verify we didn't loose the last character
    gps_assert(dump_tree_model(model, LOCATION_COLUMN)[0][-1],
               "5",
               "The last character in the Call Stack is wrong")
