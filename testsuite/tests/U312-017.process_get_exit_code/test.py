"""
Test that GPS.Process.get_exit_code works
"""

from gs_utils.internal.utils import run_test_driver, gps_assert


@run_test_driver
def driver():
    p = GS.Process("empty")
    p.get_result()
    gps_assert(p.get_exit_status(), -1, "No error code")
