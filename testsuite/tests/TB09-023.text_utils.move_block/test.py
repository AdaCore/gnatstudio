"""
Test text_utils.move_block
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)
    expected = buf.get_chars()

    # Go to the line 2 and move it
    buf.current_view().goto(buf.at(2, 1))
    text_utils.move_block(0)
    gps_assert(buf.get_chars().splitlines()[1],
               "begin",
               "Issue with move line +0")
    text_utils.move_block(3)
    gps_assert(buf.get_chars().splitlines()[1],
               "   begin",
               "Issue with move line +3")
    text_utils.move_block(-2)
    gps_assert(buf.get_chars().splitlines()[1],
               " begin",
               "Issue with move line -2")
    text_utils.move_block(-10)
    gps_assert(buf.get_chars().splitlines()[1],
               "begin",
               "Issue with move line -10")

    # Select the whole buffer and move it
    buf.select()
    text_utils.move_block(0)
    gps_assert(buf.get_chars(),
               expected,
               "Issue with move block +0")
    text_utils.move_block(3)
    gps_assert(buf.get_chars().rstrip(),
               "   " + "\n   ".join(expected.splitlines()).rstrip(),
               "Issue with move block +3")
    text_utils.move_block(-2)
    gps_assert(buf.get_chars().rstrip(),
               " " + "\n ".join(expected.splitlines()).rstrip(),
               "Issue with move block -2")
