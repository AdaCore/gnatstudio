"""
Verify that the file permission are correctly detected by GPS.
"""

from GPS import *
from gps_utils.internal.utils import *
import os


def check_permission(buf, expected, msg):
    # Need to focus-in to refresh the permission
    GPS.execute_action("open Outline")
    click_in_text(buf.at(1, 1))
    yield wait_idle()
    gps_assert(buf.is_read_only(), expected, "Issue " + msg)


@run_test_driver
def run_test():
    file = GPS.File("foo.adb")
    buf = GPS.EditorBuffer.get(file)
    path = file.path

    yield check_permission(buf, False, "when opened")

    # Set non writable via python
    os.chmod(path, 0o444)
    yield check_permission(buf, True, "with python API")

    # Unset the status via GPS API
    buf.set_read_only(False)
    yield check_permission(buf, False, "with GPS set_read_only")

    # Set the status via Python
    os.chmod(path, 0o444)
    yield check_permission(buf, True, "with python after GPS set")
