"""
Test whether breakpoints can be set/deleted in the assembly view.
"""
import GPS
from gs_utils.internal.utils import *

INST_COLUMN = 2
NUMBER_COLUMN = 3


@run_test_driver
def test_driver():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(6, 1))
    yield wait_idle()
    GPS.execute_action("debug set line breakpoint")
    yield wait_idle()

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    debug = GPS.Debugger.get()

    # Open the assembly view
    GPS.execute_action("open assembly view")
    yield wait_for_mdi_child("Assembly")
    yield wait_idle()

    # Check that the assembly view is empty
    assembly = GPS.MDI.get("Assembly").pywidget()
    model = get_widgets_by_type(Gtk.TreeView, assembly)[0].get_model()
    chars = model.get_value(model.get_iter_first(), INST_COLUMN)
    gps_assert(
        chars.find("get assembly code") == -1,
        False,
        "The Assembly view should be empty",
    )

    # start to have the valid address
    debug.send("run")
    yield wait_DAP_server("disassemble")
    yield wait_idle()

    # Check that the assembly view is not empty after the first stop
    chars = model.get_value(model.get_iter_first(), INST_COLUMN)
    gps_assert(
        chars.find("get assembly code") == -1,
        True,
        "The Assembly view should be filled",
    )

    # Open the Breakpoints view
    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child("Breakpoints")

    # Clear breakpoints
    yield wait_until_not_busy(debug)
    GPS.execute_action("debug clear breakpoints")
    yield wait_until_not_busy(debug)

    # Check that no breakpoints has been set
    view = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    gps_assert(
        dump_tree_model(model, NUMBER_COLUMN),
        [],
        "The Breakpoints view should be empty",
    )

    # Create a breakpoint in the Assembly view
    GPS.execute_action("assembly_view toggle breakpoint")
    yield wait_until_not_busy(debug)
    gps_assert(
        dump_tree_model(model, NUMBER_COLUMN),
        [" 2"],
        "The Breakpoints view should have one breakpoint",
    )

    # Delete the breakpoint in the Assembly view
    GPS.execute_action("assembly_view toggle breakpoint")
    yield wait_until_not_busy(debug)
    gps_assert(
        dump_tree_model(model, NUMBER_COLUMN),
        [],
        "The Breakpoints view should be empty",
    )
