"""
This test checks that the Analysis report view is correctly spawned
after running GNATmetric on the current file.
"""

from GPS import *
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    buffer = EditorBuffer.get(File("main.adb"))
    GPS.BuildTarget("GNAT Metrics for file").execute(force=True)
    gps_assert(GPS.MDI.get("Analysis Report") is not None,
               True,
               "The Analysis view should be spawned")
