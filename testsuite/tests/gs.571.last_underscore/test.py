"""
Verify that we don't crash on saving a file named
with an underscore at the end.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    main_adb = GPS.File("main.adb_")
    main_buffer = GPS.EditorBuffer.get_new()
    main_buffer.save(False, main_adb)
    yield wait_idle()
