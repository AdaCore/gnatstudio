"""
Test the autodection of clearcase. (This is using a fake clearcase executable)
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    gps_assert(GPS.VCS2.active_vcs().name, "clearcase native", "Autodection failed")
