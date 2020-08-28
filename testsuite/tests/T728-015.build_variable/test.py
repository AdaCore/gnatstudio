import GPS
from gs_utils.internal.utils import *

"""
Verify that external variable is escaped

"""


@run_test_driver
def test_driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.BuildTarget("Build All").execute(force=True)
    yield wait_tasks()

    gps_assert('gprbuild: "baz bar"' in GPS.Console("Messages").get_text(), False)
