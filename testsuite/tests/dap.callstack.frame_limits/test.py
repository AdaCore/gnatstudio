"""
Simple test on the callstack: verify that the `debug callstack fetch` action works.
"""
import GPS
from gs_utils.internal.utils import *
from workflows import promises


@run_test_driver
def test_driver():
    NAME_COLUMN = 1

    GPS.Preference("debugger-frames-limit").set(2)
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    p = promises.DebuggerWrapper(GPS.File("main"))
    d = p.get()
    yield wait_until_not_busy(d)
    yield p.send_promise("b main.adb:10")
    yield wait_until_not_busy(d)

    GPS.execute_action("open debugger call stack")
    yield wait_for_mdi_child("Call Stack")
    view = GPS.MDI.get("Call Stack")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    yield wait_idle()

    yield p.send_promise("run")
    yield wait_DAP_server('stackTrace')

    # Verify the view was correctly updated by the run/break command
    gps_assert(dump_tree_model(model, NAME_COLUMN),
               ["main.one.two.three.four",
                "main.one.two.three"],
               "Wrong content after breaking")

    GPS.execute_action("debug callstack fetch")
    yield wait_DAP_server("stackTrace")
    yield wait_idle()
    gps_assert(dump_tree_model(model, NAME_COLUMN),
               ["main.one.two.three.four",
                "main.one.two.three",
                "main.one.two",
                "main.one"],
               "Wrong content after the first fetch")

    GPS.execute_action("debug callstack fetch")
    yield wait_DAP_server("stackTrace")
    yield wait_idle()
    gps_assert(dump_tree_model(model, NAME_COLUMN),
               ["main.one.two.three.four",
                "main.one.two.three",
                "main.one.two",
                "main.one",
                "main"],
               "Wrong content after the next fetch")

