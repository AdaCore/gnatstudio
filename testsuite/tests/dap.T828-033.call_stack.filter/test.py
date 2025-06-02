"""
Test the filter in the Call Stack view.
"""

import platform
import GPS
from gs_utils.internal.utils import *
from workflows import promises


@run_test_driver
def test_driver():
    yield wait_tasks()
    GPS.execute_action("Build & Debug Number 1")
    yield wait_for_mdi_child("Debugger Console")
    yield timeout(500)

    p = promises.DebuggerWrapper(GPS.File("main"))
    d = p.get()
    for s in ["b hidden.adb:8", "run"]:
        yield wait_until_not_busy(d)
        yield p.send_promise(s)

    yield wait_DAP_server('stackTrace')

    win = GPS.MDI.get("Call Stack").pywidget()
    tree = get_widgets_by_type(Gtk.TreeView, win)[0]
    selection = tree.get_selection()

    host_name = GPS.Process("hostnamectl").get_result()

    # Filter the Call Stack
    get_widget_by_name("Call Stack Filter").set_text("pack.foo")
    yield timeout(500)
    tree_model_dump = dump_tree_model(tree.get_model(), 0)
    gps_assert(
        dump_tree_model(tree.get_model(), 0),
        ["1"],
        "Incorrect Callstack tree when filtered",
    )

    # Frame 0 is filtered out => it should select nothing
    yield p.send_promise("frame 0")
    yield wait_until_not_busy(d)
    gps_assert(
        selection.get_selected()[1], None, "This frame is hidden and can't be selected"
    )

    # Frame 0 is filtered out => it should select nothing
    yield p.send_promise("frame 0")
    yield wait_until_not_busy(d)
    gps_assert(
        selection.get_selected()[1], None, "This frame is hidden and can't be selected"
    )

    # Frame 1 is visible => it should select it
    yield p.send_promise("frame 1")
    yield wait_until_not_busy(d)
    model, iter = selection.get_selected()
    gps_assert(
        model.get_value(iter, 0),
        "1",
        "This frame is visible and should be selected",
    )
