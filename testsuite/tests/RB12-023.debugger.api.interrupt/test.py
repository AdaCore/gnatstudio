"""
Test whether debugger.interrupt works.
"""

import GPS
from gps_utils.internal.utils import *


@run_test_driver
def driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    debuger = GPS.Debugger.get()
    debuger.non_blocking_send("run")
    yield timeout(20)
    debuger.interrupt()

    yield wait_idle()
    gps_assert(debuger.is_busy(), False,
               "The debugger should be stopped at this time")
    debuger.close()
