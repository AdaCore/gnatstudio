"""
Check if GPS.EditorBuffer.get_analysis_unit work
"""

from GPS import *
from gps_utils.internal.utils import *
import traceback


@run_test_driver
def run_test():
    ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
    unit = ed.get_analysis_unit()
    sloc = str(unit.root.sloc_range)
    gps_assert(sloc, "1:1-5:7")
