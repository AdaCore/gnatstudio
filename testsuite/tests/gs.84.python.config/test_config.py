"""
Test GPS.Project.get_config.
"""

from gs_utils.internal.utils import *


@run_test_driver
def driver():
    yield wait_idle()
    gps_assert(
        GPS.Project.get_config().base_name(),
        "config.txt",
        "Wrong config when config is set",
    )
    gps_assert(
        GPS.Project.get_autoconfig(), None, "Wrong autoconfig when config is set"
    )
