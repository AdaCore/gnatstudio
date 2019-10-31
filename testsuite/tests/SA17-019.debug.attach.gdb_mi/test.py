"""
This test checks that GS correctly attaches to the process
without debug information
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("/Debug/Initialize/no main file")
    yield wait_tasks(other_than=known_tasks)

    GPS.Console("Messages").clear()
    yield idle_modal_dialog(lambda: GPS.execute_action("debug attach"))
    d = get_window_by_title("Select the process to attach to")
    get_widgets_by_type(Gtk.Entry, d)[0].set_text(os.environ['TESTPID'])
    get_stock_button(d, Gtk.STOCK_OK).clicked()
    yield wait_tasks(other_than=known_tasks)
    e = GPS.Debugger.get().get_executable()
    t = GPS.Console("Messages").get_text().replace(
            "There is no debug information for this frame.\n", "")
    gps_assert(t, "",
               "Wrong content of the Messages view '{}'".format(
                   GPS.Console("Messages").get_text()))
