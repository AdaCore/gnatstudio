from gs_utils.internal.utils import *


@run_test_driver
def driver():
    p = "/Analyze/Metrics/Compute Metrics on Current Project"
    gps_assert(GPS.Menu.get(p).action.can_execute(),
               True,
               p + " should be active")

    sp = "/Analyze/Metrics/Compute Metrics on Current Project & Subprojects"
    gps_assert(GPS.Menu.get(sp).action.can_execute(),
               True,
               sp + " should be active")

    f = "/Analyze/Metrics/Compute Metrics on Current File"
    gps_assert(GPS.Menu.get(f).action.can_execute(),
               False,
               f + " should be inactive")
