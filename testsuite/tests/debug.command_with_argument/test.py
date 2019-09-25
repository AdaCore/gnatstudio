"""
Commands with arguments (for example: next 4) should properly update the
Variables view.
"""
import GPS
from gps_utils.internal.utils import *

VALUE_COLUMN = 1


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(6, 1))
    GPS.execute_action("debug set line breakpoint")

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    GPS.execute_action("open debugger variables window")
    yield wait_for_mdi_child("Variables")
    view = GPS.MDI.get("Variables")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()

    # Add the variable Foo
    debug = GPS.Debugger.get()
    debug.send("graph display Foo")
    yield wait_until_not_busy(debug)
    # Run and verify the value
    debug.send("run")
    yield wait_until_not_busy(debug)
    gps_assert(dump_tree_model(model, VALUE_COLUMN),
               ['1'],
               "Wrong value after break")
    # Check the next command alone
    debug.send("next")
    yield wait_until_not_busy(debug)
    gps_assert(dump_tree_model(model, VALUE_COLUMN),
               ['2'],
               "Wrong value after break")
    # Check the next command with an argument
    debug.send("next 4")
    yield wait_until_not_busy(debug)
    gps_assert(dump_tree_model(model, VALUE_COLUMN),
               ['6'],
               "Wrong value after break")

    # Clean the Variables view between different debugger runs
    GPS.execute_action("debug tree clear")
