"""
Test whether breakpoints can be set/deleted in the assembly view.
"""
import GPS
from gs_utils.internal.utils import *

NUMBER_COLUMN = 0


@run_test_driver
def test_driver():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(6, 1))
    GPS.execute_action("debug set line breakpoint")

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    debug = GPS.Debugger.get()

    # start to have the valid address
    debug.start()
    yield hook('debugger_location_changed')

    # Open the Breakpoints view
    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child('Breakpoints')

    # Clear breakpoints
    yield wait_until_not_busy(debug)
    GPS.execute_action("debug clear breakpoints")
    yield wait_until_not_busy(debug)

    # Check that no breakpoints has been set
    view = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    gps_assert(
        dump_tree_model(model, NUMBER_COLUMN), [],
        "The Breakpoints view should be empty")

    # Open the assembly view
    GPS.execute_action("open assembly view")
    yield wait_for_mdi_child("Assembly")
    yield wait_idle()

    # Create a breakpoint in the Assembly view
    GPS.execute_action("assembly_view toggle breakpoint")
    yield wait_until_not_busy(debug)
    gps_assert(dump_tree_model(model, NUMBER_COLUMN),
               [" 2"],
               "The Breakpoints view should have one breakpoint")

    # Delete the breakpoint in the Assembly view
    GPS.execute_action("assembly_view toggle breakpoint")
    yield wait_until_not_busy(debug)
    gps_assert(
        dump_tree_model(model, NUMBER_COLUMN), [],
        "The Breakpoints view should be empty")
