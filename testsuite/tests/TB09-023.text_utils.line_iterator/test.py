"""
Test text_utils.LineIterator class
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    expected = buf.get_chars().splitlines()
    cpt = 0
    for start, end in text_utils.LineIterator(
        buf.beginning_of_buffer(), buf.end_of_buffer()
    ):
        # The end location include the EOL character: ignore it via rstrip
        gps_assert(buf.get_chars(start, end).rstrip(), expected[cpt], "Wrong line")
        cpt += 1
