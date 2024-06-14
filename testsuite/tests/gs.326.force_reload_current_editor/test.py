"""
Tests that GPS.EditorBuffer.get(force=True) will refresh the editor
with the content from the disk even if GPS.File has not been given.
"""

import os
import os.path
import os_utils
from gs_utils.internal.utils import run_test_driver, gps_assert, wait_idle


FILE = "foo.adb"


@run_test_driver
def driver():
    gps_f = GPS.File(FILE)
    buf = GPS.EditorBuffer.get(gps_f)
    gps_assert(buf.get_chars(), "a\n", "Wrong content when opening the file")

    # Don't save the buffer, "b" will be discarded at the next reload
    buf.insert(buf.at(1, 2), "b")
    gps_assert(
        buf.get_chars(), "ab\n", "Wrong content when editing the file via GS API"
    )

    with open(FILE, "w") as f:
        f.write("ac\n")

    GPS.EditorBuffer.get(gps_f, force=True)
    yield wait_idle()
    gps_assert(buf.get_chars(), "ac\n", "Wrong content when reloading the GPS.File")

    with open(FILE, "w") as f:
        f.write("acd\n")

    GPS.EditorBuffer.get(force=True)
    yield wait_idle()
    gps_assert(
        buf.get_chars(), "acd\n", "Wrong content when reloading the current buffer"
    )
