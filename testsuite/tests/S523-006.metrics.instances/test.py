"""
This test checks that the Metrics view is correctly spawned
after running GNATmetric on the current file.
"""

from GPS import *
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    buffer = EditorBuffer.get(File("main.adb"))
    GPS.BuildTarget("GNAT Metrics for file").execute(
        force=True,
        extra_args='--lines-all --syntax-all --complexity-all '
        + '--coupling-all')
    gps_assert(GPS.XMLViewer.get_existing('Metrics') is not None,
               True,
               "The Metrics view should be spawned")
