"""
Test GPS.Project.get_autoconfig.
"""

from gs_utils.internal.utils import *


@run_test_driver
def driver():
    gps_assert(GPS.Project.get_config(),
               None,
               "Wrong config when autoconfig is set")
    gps_assert(GPS.Project.get_autoconfig().base_name(),
               "auto.cgpr",
               "Wrong autoconfig when autoconfig is set")
