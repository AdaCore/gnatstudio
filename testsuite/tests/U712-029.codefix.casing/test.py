"""
Test the creation of codefix for bad casing
"""

from gs_utils.internal.utils import *
from GPS import *


@run_test_driver
def run():
    # Disable auto jump to first => it will prevent opening the first file
    # with a build message (Thus no file should be opened)
    GPS.Preference("locations-auto-jump-to-first").set(False)
    GPS.execute_action("Build Main Number 1")
    yield wait_tasks()
    gps_assert(len(GPS.Message.list()), 6, "Missing messages for bad casing")
    gps_assert(GPS.MDI.get("foo.adb"), None, "foo.adb should not be opened")
    gps_assert(GPS.MDI.get("bar.adb"), None, "bar.adb should bot be opened")
    gps_assert(GPS.MDI.get("bar.ads"), None, "bar.ads should bot be opened")
