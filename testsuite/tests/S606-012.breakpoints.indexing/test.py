"""
Test the breakpoints indexing when creating and deleting breakpoints inside
and outside a debugger session + the transition (starting/closing a debugging
session).
"""
import GPS
from gs_utils.internal.utils import *

ID_COLUMN = 0
LINE_COLUMN = 5
MAIN = "main.adb"


def add_breakpoint(buf, line):
    buf.current_view().goto(buf.at(line, 1))
    GPS.execute_action("debug set line breakpoint")


def to_list(num_list):
    """Format the list to the output of dump_tree_model"""
    return [' ' + str(num) for num in num_list]


def check_breakpoints(model, id_list, lines_list, msg):
    gps_assert(dump_tree_model(model, ID_COLUMN), to_list(id_list),
               "Invalid id when " + msg)
    gps_assert(dump_tree_model(model, LINE_COLUMN), to_list(lines_list),
               "Invalid lines when " + msg)


@run_test_driver
def test_driver():
    main = GPS.File(MAIN)
    buf = GPS.EditorBuffer.get(main)
    add_breakpoint(buf, 8)
    add_breakpoint(buf, 11)
    add_breakpoint(buf, 12)

    # Open the Breakpoints view and check that the breakpoints has been set
    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child('Breakpoints')
    view = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    check_breakpoints(model,
                      [1, 2, 3],
                      [8, 11, 12],
                      "adding breakpoints via the editors")

    # Launch the debugger, modify the breakpoints list and quit
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    debug = GPS.Debugger.get()
    debug.break_at_location(main, 4)
    yield wait_until_not_busy(debug)
    debug.send("run")
    yield wait_until_not_busy(debug)

    check_breakpoints(model,
                      [1, 2, 3, 4],
                      [8, 11, 12, 4],
                      "starting the debugger")
    gps_assert(debug.current_line, 4,
               "The debugger should stop at the newly created breakpoint")

    # Deleting breakpoints should update the list
    debug.send("delete 2 3")
    yield wait_until_not_busy(debug)
    check_breakpoints(model,
                      [1, 4],
                      [8, 4],
                      "deleting breakpoints via the debugger")

    # Closing the debugger should reset the index
    debug.close()
    check_breakpoints(model,
                      [1, 2],
                      [8, 4],
                      "closing the debugger")

    # Add new breakpoints after closing the debugger session
    # Give the focus to the buffer, "debug set line breakpoint" need it.
    GPS.MDI.get("main.adb").raise_window()
    add_breakpoint(buf, 11)
    check_breakpoints(model,
                      [1, 2, 3],
                      [8, 4, 11],
                      "creating new breakpoints after closing the debugger")

    # Close the Breakpoints view and reopen it: the index should be the same
    view.close()
    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child('Breakpoints')
    view = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    # The view was closed: retrieve the new model
    model = tree.get_model()
    check_breakpoints(model,
                      [1, 2, 3],
                      [8, 4, 11],
                      "closing and reopening the view")

    # Deleting a breakpoint outside a debugger session should reindex them
    # Attention: placing the cursor in a line containing a breakpoint
    # will select it in the Breakpoints view in the next loop iteration
    # Thus to stabilise the text, we will also do a manual selection
    buf.current_view().goto(buf.at(4, 1))
    selection = tree.get_selection()
    selection.select_path(1)
    GPS.execute_action("debug delete breakpoint")
    view = GPS.MDI.get("Breakpoints")
    check_breakpoints(model,
                      [1, 2],
                      [8, 11],
                      "deleting a breakpoint outside a debugging session")
