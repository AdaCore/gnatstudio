"""
This test verifies that the variables view displays arrays properly
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = b.current_view()
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    d = GPS.Debugger.get()
    for s in ["b main.adb:23",
              "b main.adb:27",
              "run"]:
        d.send(s)
        yield wait_until_not_busy(d)

    variables = Variables_View()
    yield variables.open_and_yield()
    Variables_View.display("Matrix_3")
    yield wait_idle()

    for s in ["step",
              "finish"]:
        d.send(s)
        yield wait_until_not_busy(d)
    yield wait_idle()

    variables.expand([0])
    tree = get_widget_by_name("Variables Tree")
    iter = tree.get_model().get_iter("0:0")
    v = tree.get_model().get_value(iter, 1)
    gps_assert(v == "0.0", True, "Array value is incorrect:" + v)
