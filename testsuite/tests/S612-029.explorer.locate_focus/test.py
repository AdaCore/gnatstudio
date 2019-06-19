"""
Test the focus after the actions "Locate file in explorer"
and "Locate file in explorer (no focus)".
"""

from GPS import *
from gps_utils.internal.utils import *

FILE = "foo.adb"


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File(FILE))
    gps_assert(GPS.MDI.current().name(short=True),
               FILE,
               "Wrong focus when opening the file")
    GPS.execute_action("Locate file in explorer (no focus)")
    yield wait_tasks(other_than=known_tasks)
    gps_assert(GPS.MDI.current().name(short=True),
               FILE,
               "The focus should not have changed")
    GPS.execute_action("Locate file in explorer")
    yield wait_tasks(other_than=known_tasks)
    gps_assert(GPS.MDI.current().name(short=True),
               "Project",
               "The focus should have changed")
