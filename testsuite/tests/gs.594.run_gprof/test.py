"""Check that gprof mode works properly"""

import os.path
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    GPS.set_build_mode("gprof")
    GPS.execute_action("Build All")
    yield wait_tasks()
    GPS.execute_action("Run Main Number 1")
    yield wait_tasks()
    gps_assert(os.path.exists("gmon.out"), True, "No `gmon.out` file")
