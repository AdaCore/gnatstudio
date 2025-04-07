"""
Tests that we don't have warning when scenario variable is set
via the command line switch.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    yield wait_tasks()
    gps_assert(
        "gnatsas-gnatls" not in GPS.Console("Messages").get_text(),
        True,
        "We should not have any warning in the Messages view",
    )
    gps_assert(
        len(GPS.Locations.list_categories()) == 0,
        True,
        "We should not have empty locations"
    )    

