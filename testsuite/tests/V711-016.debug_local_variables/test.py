""" Test how local variables/arguments is displayed """
from GPS import *
from gs_utils.internal.utils import *

expected_locals = ["", ["i = 0", "b = false"]]
expected_args = ["", ["level - 0", "i = 0", "b = false"]]


@run_test_driver
def run_test():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    GPS.MDI.get("main.adb").raise_window()
    buf.current_view().goto(buf.at(6, 1))
    GPS.execute_action("debug set line breakpoint")
    buf.current_view().goto(buf.at(12, 1))
    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks(other_than=known_tasks)

    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_until_not_busy(debug)

    GPS.execute_action("debug tree display local variables")
    tree = get_widget_by_name("Variables Tree")
    dump = dump_tree_model(tree.get_model(), 1)
    gps_assert(dump, expected_locals)
    GPS.execute_action("debug tree clear")
    GPS.execute_action("debug continue")
    yield wait_until_not_busy(debug)

    GPS.execute_action("debug tree display arguments")
    tree = get_widget_by_name("Variables Tree")
    dump = dump_tree_model(tree.get_model(), 1)
    gps_assert(dump, expected_args)
