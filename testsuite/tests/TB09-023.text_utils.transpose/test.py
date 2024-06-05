"""
Test text_utils.transpose_chars/lines
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    # Inverse character 3 and 2 in line 1
    buf.current_view().goto(buf.at(1, 3))
    text_utils.transpose_chars()
    gps_assert(
        buf.get_chars().rstrip(), "World\nHello", "Issue when transposing one character"
    )

    # Inverse line 2 and the previous = line 1
    buf.current_view().goto(buf.at(2, 1))
    text_utils.transpose_lines()
    gps_assert(
        buf.get_chars().rstrip(), "Hello\nWorld", "Issue when transposing one character"
    )
