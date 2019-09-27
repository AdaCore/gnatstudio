"""
Verify that we export only the messages that are currently visible
in the Locations view.
"""

from GPS import *
from gps_utils.internal.utils import *

@run_test_driver
def run_test():
    yield timeout(30000)
