"""
This test checks that the GPS.Process API handles correctly commands
given as unicode objects.
"""
import GPS
from gs_utils.internal.utils import run_test_driver, gps_assert

@run_test_driver
def run_test():
    GPS.Process(unicode("echo"))
    gps_assert("Could not locate executable" in GPS.Console().get_text(),
               False,
               "Unicode objects not accepted by GPS.Process")
