"""
Test text_utils.goto_word_start/end with underscore_is_word=False
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils

expected = [(1, 3), (4, 6), (8, 11), (12, 13), (14, 15), (19, 20)]


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    for start, end in expected:
        loc = buf.at(1, start)
        # start => end
        e_loc = text_utils.goto_word_end(loc, underscore_is_word=False)
        # goto_word_end is returning the last column still inside the word
        gps_assert(e_loc.column() + 1, end, "goto_word_end")
        # end => start
        s_loc = text_utils.goto_word_start(e_loc, underscore_is_word=False)
        gps_assert(s_loc.column(), start, "goto_word_start")

    # Edge cases: no infinite loop at start and end of buffer
    text_utils.goto_word_end(buf.end_of_buffer(),
                             underscore_is_word=False)
    text_utils.goto_word_start(buf.beginning_of_buffer(),
                               underscore_is_word=False)
