

"""
This test checks that GS correctly consider the debugger as started
after attaching a process.
This means that "debug continue" should not try to start the debugger again.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("/Debug/Initialize/no main file")
    yield wait_tasks(other_than=known_tasks)

    GPS.Console("Messages").clear()
    yield wait_for_mdi_child('Debugger Console')
    debugger = GPS.Debugger.get()

    debugger.send("-target-attach " + os.environ['TESTPID'])
    e = debugger.get_executable()

    GPS.execute_action("debug continue")
    yield timeout(500)

    text = debugger.get_console().get_text()
    gps_assert("[program running]" in text, True,
               "The program should have been run on 'debug continue'")
