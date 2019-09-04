"""
This test verifies that the Messages view only contains the
GPS welcome message (i.e: no error/info messages other than
that).
"""
import GPS
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    messages_lines = [l for l in GPS.Console("Messages").get_text().split('\n')
                      if l.strip()]
    gps_assert(
        len(messages_lines), 2,
        "The Messages view should not contain more than 3 lines (i.e: " +
        "the GPS welcome message")
