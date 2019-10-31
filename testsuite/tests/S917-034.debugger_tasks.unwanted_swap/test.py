"""
In the task view:
- Clicking should select the correct task
- Changing the focus and giving it back should not change the current task
"""
import GPS
from gs_utils.internal.utils import *
import re

NUMBER_COLUMN = 0
NAME_COLUMN = 5


def prepare_output(l):
    """The output for CLI or MI are different (more spaces in CLI)"""
    space_re = re.compile(" +")
    return [space_re.sub(" ", val).lstrip() for val in l]


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(23, 1))
    GPS.execute_action("debug set line breakpoint")

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_until_not_busy(debug)

    # Give the focus to the Call Stack window and open the Task view =>
    # The Debugger Tasks view will be opened in the same notebook
    GPS.execute_action("open debugger call stack")
    yield wait_for_mdi_child("Call Stack")
    GPS.MDI.get("Call Stack").raise_window()
    GPS.execute_action("open tasks debugger window")
    yield wait_for_mdi_child("Debugger Tasks")
    yield wait_until_not_busy(debug)

    view = GPS.MDI.get("Debugger Tasks")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    gps_assert(prepare_output(dump_tree_model(model, NUMBER_COLUMN)),
               ["1", "2", "* 3"],
               "Wrong task when stopping")
    yield wait_idle()

    click_in_tree(tree, "1")
    yield wait_idle()
    yield wait_until_not_busy(debug)
    gps_assert(prepare_output(dump_tree_model(model, NUMBER_COLUMN)),
               ["1", "* 2", "3"],
               "Wrong task after clicking on the row")

    # Swap the active Notebook => it should not change the current Task
    GPS.MDI.get("Call Stack").raise_window()
    GPS.MDI.get("Debugger Tasks").raise_window()
    yield wait_idle()
    gps_assert(prepare_output(dump_tree_model(model, NUMBER_COLUMN)),
               ["1", "* 2", "3"],
               "Wrong task after grabing the focus")
