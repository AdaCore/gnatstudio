"""
Test whether breakpoints can be set/deleted in the assembly view.
"""
import GPS
from gs_utils.internal.utils import *

NUMBER_COLUMN = 0


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    # Open the assembly view
    GPS.execute_action("open assembly view")
    yield wait_for_mdi_child("Assembly")

    # Open the Breakpoints view and check that no breakpoints has been set
    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child('Breakpoints')
    view = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    gps_assert(
        dump_tree_model(model, 0), [],
        "The Breakpoints view should be empty")

    # Create a breakpoint in the Assembly view
    debug = GPS.Debugger.get()
    GPS.execute_action("assembly_view toggle breakpoint")
    yield wait_until_not_busy(debug)
    gps_assert(dump_tree_model(model, NUMBER_COLUMN),
               [" 1"],
               "The Breakpoints view should have one breakpoint")

    # Delete the breakpoint in the Assembly view
    GPS.execute_action("assembly_view toggle breakpoint")
    yield wait_until_not_busy(debug)
    gps_assert(
        dump_tree_model(model, 0), [],
        "The Breakpoints view should be empty")
