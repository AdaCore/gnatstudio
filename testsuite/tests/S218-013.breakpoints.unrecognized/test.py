"""
Verify that the persistent list is correctly updated
when quitting the debugger and some unrecognized breakpoints are set.
"""
import GPS
from gps_utils.internal.utils import *

FILE_COLUMN = 4
LINE_COLUMN = 5
MAIN = "main.adb"
ALONE = "alone.adb"


def add_breakpoint(buf, line):
    buf.current_view().goto(buf.at(line, 1))
    GPS.execute_action("debug set line breakpoint")


def to_list(lines_list):
    """Format the list to the output of dump_tree_model"""
    return [' ' + str(line) for line in lines_list]


def check_breakpoints(model, files_list, lines_list, msg):
    gps_assert(dump_tree_model(model, FILE_COLUMN), files_list,
               "Invalid files when " + msg)
    gps_assert(dump_tree_model(model, LINE_COLUMN), to_list(lines_list),
               "Invalid lines when " + msg)


@run_test_driver
def test_driver():
    # Add recognized breakpoints
    main = GPS.File(MAIN)
    buf1 = GPS.EditorBuffer.get(main)
    add_breakpoint(buf1, 7)  # At line 8 when the debugger starts
    add_breakpoint(buf1, 11)
    add_breakpoint(buf1, 12)

    # Add a unrecognized breakpoint
    buf2 = GPS.EditorBuffer.get(GPS.File(ALONE))
    add_breakpoint(buf2, 3)

    # Open the Breakpoints view and check that the breakpoints has been set
    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child('Breakpoints')
    view = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    check_breakpoints(model,
                      [MAIN, MAIN, MAIN, ALONE],
                      [7, 11, 12, 3],
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
                      [MAIN, MAIN, MAIN, MAIN],
                      [8, 11, 12, 4],
                      "starting the debugger")
    gps_assert(debug.current_line, 4,
               "The debugger should stop at the newly created breakpoint")

    # Deleting breakpoints should update the list
    debug.send("delete 2 3")
    yield wait_until_not_busy(debug)
    check_breakpoints(model,
                      [MAIN, MAIN],
                      [8, 4],
                      "deleting breakpoints via the debugger")

    # Closing the debugger should add the unrecognized breakpoint
    debug.close()
    check_breakpoints(model,
                      [MAIN, MAIN, ALONE],
                      [8, 4, 3],
                      "closing the debugger")
