"""
Verify that Analyze/Metrics actions (so menu items too)
are inactive when gnatmetric is not in path
"""
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    p = "/Analyze/Metrics/Compute Metrics on Current Project"
    gps_assert(GPS.Menu.get(p).action.can_execute(), False, p + " should not be active")

    sp = "/Analyze/Metrics/Compute Metrics on Current Project & Subprojects"
    gps_assert(
        GPS.Menu.get(sp).action.can_execute(), False, sp + " should not be active"
    )
