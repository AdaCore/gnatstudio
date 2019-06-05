import GPS
from gps_utils.internal.utils import *

"""
Verify that switching between frames causes updating the variables view.

"""

@run_test_driver
def test_driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = b.current_view()
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    d = GPS.Debugger.get()
    for s in ["b main.adb:17",
              "run"]:
        d.send(s)
        yield wait_until_not_busy(d)

    d.send("graph display Var")
    yield wait_until_not_busy(d)
    yield wait_idle()

    tree = get_widget_by_name("Variables Tree")
    iter = tree.get_model().get_iter("0")
    v = tree.get_model().get_value(iter, 1)
    gps_assert(v == "true", True, "Variable value is incorrect:" + v)
    gps_assert(view.cursor().line(), 17, "Incorrect line")

    d.frame_up()
    yield wait_until_not_busy(d)
    yield wait_idle()
    iter = tree.get_model().get_iter("0")
    v = tree.get_model().get_value(iter, 1)
    gps_assert(v == "false", True, "Variable value is incorrect:" + v)
    gps_assert(view.cursor().line(), 11, "Incorrect line")
