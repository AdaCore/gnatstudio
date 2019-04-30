"""
This test check that refilling comments in .gpr files works
correctly.
"""

from GPS import *
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    buffer = EditorBuffer.get(GPS.File("default.gpr"))
    buffer.select(buffer.at(4, 1), buffer.at(5,1))
    execute_action("refill")

    gps_assert(buffer.get_chars(),
               file("default.gpr.out").read(),
               "Incorrect refilling of comments")
