"""
Verify that selection gets preserved in the
Breakpoints view when enabling/disabling 
breakpoints.
"""

import GPS
from gs_utils.internal.utils import *

NUM_COLUMN = 3
FILE_COLUMN = 4
LINE_COLUMN = 5


@run_test_driver
def test_driver():
    yield wait_tasks()
    # Open the editor and add a breakpoint
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()

    b.current_view().goto(b.at(11, 1))
    GPS.execute_action("debug set line breakpoint")
    yield wait_idle()

    b.current_view().goto(b.at(17, 1))
    GPS.execute_action("debug set line breakpoint")
    yield wait_idle()

    # Start the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()
    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)
    debug.send("run")
    yield wait_for_mdi_child("Debugger Execution main")
    yield wait_until_not_busy(debug)

    # toggle breakpoint enabled
    GPS.execute_action("open breakpoints editor")
    yield wait_idle()
    yield wait_for_mdi_child("Breakpoints")
    bp = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, bp.pywidget())[0]

    tree.get_selection().unselect_all()
    tree.get_selection().select_path("1")
    yield wait_idle()
    click_in_tree(tree, path="1")
    yield wait_DAP_server("setBreakpoints")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    click_in_tree(tree, path="1")
    yield wait_DAP_server("setBreakpoints")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    # check we have proper selection
    gps_assert(
        tree.get_selection().path_is_selected(Gtk.TreePath((1))),
        True,
        "Wrong breakpoint selected after disabling it",
    )
