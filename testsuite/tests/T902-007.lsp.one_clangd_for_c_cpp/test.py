"""
This test checks that we only spawn one instance of clangd after
loading a mixed C/C++ project.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    out = GPS.Process("pidof clangd").get_result()
    pids = out.split(' ')
    gps_assert(len(pids), 1, "There should be only one instance of clangd")
