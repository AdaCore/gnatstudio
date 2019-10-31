"""
This test checks that we correctly read -gnaty switches when opening the
Project Properties without crashing GPS.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    dialog = Project_Properties_Editor()
    yield dialog.open_and_yield()
    yield dialog.cancel()
