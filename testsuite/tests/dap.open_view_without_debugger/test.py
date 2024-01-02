"""
This test checks that we are able to open the Breakpoints view
without any exception when there is no running DAP debugger.
"""

import GPS
from gs_utils.internal.utils import *

@run_test_driver
def test_driver():
    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child('Breakpoints')
    gps_assert(GPS.MDI.get("Breakpoints") != None, True,
               "The Breakpoints view should be opened without any issue")
