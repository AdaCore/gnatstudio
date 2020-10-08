"""
This test checks that we only spawn one instance of clangd after
loading a mixed C/C++ project.
"""

import GPS
import os
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    out = GPS.Process("python count_clangds.py {}".format(os.getpid())).get_result()
    gps_assert(out, "1", "There should be only one instance of clangd")
