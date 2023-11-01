"""
Test the filter in the Call Stack view.
"""

import platform
import GPS
from gs_utils.internal.utils import *
from workflows import promises


@run_test_driver
def test_driver():
    yield wait_tasks(other_than=known_tasks)
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield timeout(300)

    p = promises.DebuggerWrapper(GPS.File("main"))
    d = p.get()
    for s in ["b hidden.adb:8",
              "run"]:
        yield wait_until_not_busy(d)
        yield p.send_promise(s)

    yield wait_until_not_busy(d)

    win = GPS.MDI.get("Call Stack").pywidget()
    tree = get_widgets_by_type(Gtk.TreeView, win)[0]
    selection = tree.get_selection()

    host_name = GPS.Process("hostnamectl").get_result()

    # The call stack could different depending on the OS
    if platform.system().lower() == 'windows':
        gps_assert(dump_tree_model(tree.get_model(), 0),
               ['0', '1', '2'],
               "Incorrect Callstack tree (Windows)")
    else:
        gps_assert(dump_tree_model(tree.get_model(), 0),
                   ['0', '1', '2', '3', '4', '5', '6'],
                   "Incorrect Callstack tree")

    # Filter the Call Stack
    get_widget_by_name("Call Stack Filter").set_text("main")
    yield timeout(500)
    yield wait_idle()

    if platform.system().lower() == 'windows':
        gps_assert(dump_tree_model(tree.get_model(), 0),
                   ['2'],
                   "Incorrect Callstack tree when filtered")
    else:
        gps_assert(dump_tree_model(tree.get_model(), 0),
               ['2', '3', '5'],
               "Incorrect Callstack tree when filtered")

    # Frame 0 is filtered out => it should select nothing
    yield p.send_promise("frame 0")
    yield wait_until_not_busy(d)
    gps_assert(selection.get_selected()[1],
               None,
               "This frame is hidden and can't be selected")

# Uncomment when Task #147 is fixed

#    if platform.system().lower() == 'windows':
#        # Frame 1 is visible => it should select it
#        yield p.send_promise("frame 2")
#        yield wait_until_not_busy(d)
#        model, iter = selection.get_selected()
#        gps_assert(model.get_value(iter, 0),
#                   "2",
#                   "This frame is visible and should be selected")
#    else:
#        # Frame 2 is visible => it should select it
#        yield p.send_promise("frame 2")
#        yield wait_until_not_busy(d)
#        model, iter = selection.get_selected()
#        gps_assert(model.get_value(iter, 0),
#                   "2",
#                   "This frame is visible and should be selected")

    d.send('q')
    yield wait_tasks()
