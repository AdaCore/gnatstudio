"""
This test checks that GS correctly attaches to the process
without debug information
"""
import GPS
from gps_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("/Debug/Initialize/no main file")
    yield wait_tasks(other_than=known_tasks)

    GPS.Console("Messages").clear()
    yield wait_for_mdi_child('Debugger Console')
    debugger = GPS.Debugger.get()
    debugger.send("-target-attach " + os.environ['TESTPID'])
    e = debugger.get_executable()

    t = GPS.Console("Messages").get_text().replace(
            "There is no debug information for this frame.\n", "")
    gps_assert(t, "",
               "Wrong content of the Messages view '{}'".format(
                   GPS.Console("Messages").get_text()))
