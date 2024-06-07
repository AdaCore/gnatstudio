"""
This test checks that we don't get any STORAGE_ERROR when opening a very large
file.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.EditorBuffer.get(GPS.File("libadalang-implementation.adb"))
    yield wait_tasks()
