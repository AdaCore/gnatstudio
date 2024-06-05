"""
Simple test of Goto decl: the buffer should be opened at the end.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    view = buf.current_view()
    yield wait_tasks(other_than=known_tasks)
    view.goto(buf.at(5, 10))

    GPS.execute_action("goto declaration")
    yield wait_tasks()

    gps_assert(GPS.MDI.get("bar.ads") is not None, True, "The buffer should be opened")
