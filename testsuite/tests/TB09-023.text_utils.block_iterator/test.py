"""
Test text_utils.BlockIterator class
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    # Use BlockIterator to navigate/count the different highlighing overlays
    cpt = 0
    for start, end in text_utils.BlockIterator(buf, "comment"):
        cpt += 1
    gps_assert(cpt, 6, "Wrong number of commented lines")

    cpt = 0
    for start, end in text_utils.BlockIterator(buf, "string"):
        cpt += 1
    gps_assert(cpt, 2, "Wrong number of strings")

    cpt = 0
    for start, end in text_utils.BlockIterator(buf, "type"):
        cpt += 1
    gps_assert(cpt, 3, "Wrong number of types")

    cpt = 0
    for start, end in text_utils.BlockIterator(buf, "keyword"):
        cpt += 1
    gps_assert(cpt, 16, "Wrong number of types")

    s_loc = buf.at(3, 3)
    e_loc = buf.at(4, 4)
    buf.select(s_loc, e_loc)
    # Special behavior for selection: the result is unique
    for start, end in text_utils.BlockIterator(buf, "selection"):
        gps_assert(start, s_loc, "Wrong start location for selection")
        gps_assert(end, e_loc, "Wrong end location for selection")

    buf.current_view().goto(buf.at(5, 13))
    # Special behavior for words: return the current word boundaries
    for start, end in text_utils.BlockIterator(buf, "word"):
        gps_assert(start, buf.at(5, 9), "Wrong start location for word")
        gps_assert(end, buf.at(5, 17), "Wrong end location for word")
