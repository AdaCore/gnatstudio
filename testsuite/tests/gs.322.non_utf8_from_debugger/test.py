"""This test verifies that the debug output view is able to
   interpret a complex string that mixes UTF-8 with escape
   sequences and line breaks.

   We do this by launching a specially crafted C program that
   is able to print the non-buffered special string.
"""
from gs_utils.internal.utils import run_test_driver, wait_until_true, dot_exe, timeout

@run_test_driver
def driver():
    # Build and debug
    GPS.execute_action("Build & Run Number 1")
    # Wait until the run window has appeared
    yield wait_until_true(lambda: GPS.MDI.get("Run: t" + dot_exe) != None)
    # wait a bit for the output window to print the output
    yield timeout(1000)
