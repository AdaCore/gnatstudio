"""
Test the "ignore breakpoint" commmand.
"""
import GPS
from gs_utils.internal.utils import *

VALUE_COLUMN = 1


@run_test_driver
def test_driver():
    # Start the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # Open the Variables view
    GPS.execute_action("open debugger variables window")
    yield wait_for_mdi_child("Variables")
    view = GPS.MDI.get("Variables")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()

    # Monitor the variable X
    debug = GPS.Debugger.get()
    debug.send("graph display X")
    yield wait_until_not_busy(debug)

    # Open the editor and add a breakpoint on the line where X's value change
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = b.current_view()
    view.goto(b.at(5, 1))
    yield wait_idle()
    GPS.execute_action("debug set line breakpoint")
    yield wait_idle()

    # Run the debugger and hit the breakpoint for the first time
    debug.send("run")
    yield wait_DAP_server("variables")
    gps_assert(dump_tree_model(model, VALUE_COLUMN), ["0"], "Wrong value after run")

    # Continuing should increase the value by 1
    debug.send("continue")
    yield wait_DAP_server("variables")
    gps_assert(
        dump_tree_model(model, VALUE_COLUMN),
        ["1"],
        "Wrong value after continuing a single time",
    )

    # Continuing 3 more times
    debug.send("continue 3")
    yield wait_DAP_server("variables")
    gps_assert(
        dump_tree_model(model, VALUE_COLUMN),
        ["4"],
        "Wrong value after continuing multiple time",
    )

    # Ignore 3 times the breakpoint
    debug.send("ignore 1 3")
    debug.send("continue")
    yield wait_DAP_server("variables")
    gps_assert(
        dump_tree_model(model, VALUE_COLUMN), ["8"], "Wrong value after ignoring bp"
    )

    # The breakpoint should behave normally now
    debug.send("continue")
    yield wait_DAP_server("variables")
    gps_assert(
        dump_tree_model(model, VALUE_COLUMN),
        ["9"],
        "Wrong value after continue when stopping to ignore bp",
    )
