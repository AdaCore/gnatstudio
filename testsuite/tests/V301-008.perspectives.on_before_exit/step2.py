"""
Step 2: the file should have been saved in perspectives6.xml
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    gps_assert(GPS.MDI.get("main.adb") is not None,
               True,
               "perspectives6.xml was not properly updated")
