"""Test that relocate-build-tree switch works"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    yield wait_tasks()
    gps_assert(
        GPS.Console("Messages").get_text().find("obj"),
        -1,
        GPS.Console("Messages").get_text(),
    )
