"""
Verify that we can jump to a breakpoint location without issue.
"""
import GPS
from gs_utils.internal.utils import *

line = 25
col = 1


def check_action():
    """
    The action should open the breakpoint's file and jump to the its loc
    """
    # Open the Breakpoints view and select the breakpoint, this is necessary
    # to run the action "debug view breakpoint"

    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child("Breakpoints")
    view = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    iter = model.get_iter_first()
    tree.get_selection().select_iter(iter)

    GPS.execute_action("debug view breakpoint")
    yield wait_idle()

    buf = GPS.EditorBuffer.get(GPS.File("main.adb"), open=False)
    if buf:
        loc = buf.get_cursors()[0].location()
        gps_assert(loc.line(), line, "Wrong line")
        gps_assert(loc.column(), col, "Wrong column")
        # Close it for the next call
        buf.close()
    else:
        simple_error("The buffer should be opened")


@run_test_driver
def test_driver():
    # Open main.adb and set a breakpoint line 25 and close it
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    buf.current_view().goto(buf.at(line, col))

    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks(other_than=known_tasks)
    buf.close()

    # Check the action when there is no debugger session
    yield check_action()

    # Launch the debugger

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # Check the action when the debugger session is started
    yield check_action()
