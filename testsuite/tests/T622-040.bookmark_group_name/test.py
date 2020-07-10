"""Checking whether name with aliases like %M does not raise an exception
"""
from GPS import *
from gs_utils.internal.utils import *
from gs_utils.internal.dialogs import *


@run_test_driver
def run_test():
    # Open the file
    buf = GPS.EditorBuffer.get(GPS.File("hello.adb"))

    # Create group
    GPS.Bookmark.create_group ("group for %M")
    yield wait_idle()

