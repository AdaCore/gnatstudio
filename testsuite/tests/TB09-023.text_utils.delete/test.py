"""
Test text_utils.delete_forward/deleted_line
Also test beginning_of_buffer and end_of_line to move the cursor
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils

expected = ["AAAAA", "BBBBB"]


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    # Go to the beginning of the buffer
    text_utils.beginning_of_buffer()
    gps_assert("\n".join(expected),
               buf.get_chars().rstrip(),
               "Wrong initial buffer")

    # Test delete_forward
    text_utils.delete_forward()
    gps_assert("\n".join([expected[0][1:], expected[1]]),
               buf.get_chars().rstrip(),
               "Issue with delete_forward")

    # Test delete_line
    text_utils.delete_line()
    gps_assert(expected[1],
               buf.get_chars().rstrip(),
               "Issue with delete_line")

    # Move to a non existent line and delete it => should go to the last line
    text_utils.end_of_line("foo.adb", 1)
    text_utils.delete_line()
    gps_assert("",
               buf.get_chars().rstrip(),
               "Issue with end_of_line")
