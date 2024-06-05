"""
Test text_utils.WordIterator class
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    iterator = text_utils.WordIterator(buf.beginning_of_buffer(), buf.end_of_buffer())
    expected = buf.get_chars().split()
    cpt = 0
    for start, end in iterator:
        gps_assert(buf.get_chars(start, end), expected[cpt], "Wrong word")
        cpt += 1

    # Move the start of the iterator to the last word
    iterator.starts_at(buf.at(1, 11))
    for start, end in iterator:
        gps_assert(buf.get_chars(start, end), expected[-1], "Issue with start_at")
