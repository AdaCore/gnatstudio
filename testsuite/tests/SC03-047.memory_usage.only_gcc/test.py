"""
This test checks that the ld plugin for the Memory Usage View is not
active when the toolchain is not GCC-based.
"""

import GPS
from gs_utils.internal.utils import *

@run_test_driver
def run_test():
    gps_assert(GPS.MDI.get('Memory Usage'), None,
               "The Memory Usage View shuould not be opened")
