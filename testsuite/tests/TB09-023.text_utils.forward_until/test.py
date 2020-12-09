"""
Test text_utils.forward_until
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    loc = buf.at(1, 7)
    new_loc = text_utils.forward_until(
        loc,
        lambda c: c == "_")
    gps_assert(new_loc.column(), 7, "Wrong column for default")
    loc = buf.at(1, 7)
    new_loc = text_utils.forward_until(
        loc,
        lambda c: c == "_",
        skip_first_char=True)
    gps_assert(new_loc.column(), 13, "Wrong column for SKIP_FIRST_CHAR=TRUE")
    loc = buf.at(1, 7)
    new_loc = text_utils.forward_until(
        loc,
        lambda c: c == "_",
        backwards=True)
    gps_assert(new_loc.column(), 7, "Wrong column for BACKWARDS=TRUE")
    loc = buf.at(1, 14)
    new_loc = text_utils.forward_until(
        loc,
        lambda c: c == "_",
        stop_at_eol=False)
    gps_assert(new_loc.column(), 1, "Wrong column for EOL=FALSE")
    loc = buf.at(1, 14)
    new_loc = text_utils.forward_until(
        loc,
        lambda c: c == "_",
        stop_at_eol=True)
    gps_assert(new_loc.column(), 14, "Wrong column for EOL=TRUE")
    loc = buf.at(2, 2)
    new_loc = text_utils.forward_until(
        loc,
        lambda c: c == "_",
        give_up=False)
    gps_assert(new_loc.line(), 3, "Wrong column for GIVE_UP=FALSE")
    gps_assert(new_loc.column(), 1, "Wrong column for GIVE_UP=FALSE")
    loc = buf.at(2, 2)
    new_loc = text_utils.forward_until(
        loc,
        lambda c: c == "_",
        stop_at_eol=True,
        give_up=False)
    gps_assert(new_loc.line(), 2,
               "Wrong line for GIVE_UP=FALSE & EOL=TRUE")
    gps_assert(new_loc.column(), 5,
               "Wrong column for GIVE_UP=FALSE & EOL=TRUE")
