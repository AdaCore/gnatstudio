"""The completion should be canceled by action moving the cursor as
'goto beginning of line'.
"""
from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    loc = buf.at(1, 0)
    buf.insert(loc, "pr")
    GPS.execute_action("Complete identifier (advanced)")
    expected = buf.get_chars()
    GPS.execute_action("goto beginning of line")
    send_key_event(GDK_TAB)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.get_chars(),
               expected,
               "The completion should have been canceled")
