"""
Test the autocasing while inserting characters and especially a new line.
"""
from gs_utils.internal.utils import *


EXPECTED = "      if A and then"


@run_test_driver
def driver():
    GPS.Preference("Ada-Casing-Policy").set("On_The_Fly")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(5, 21))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    start_loc = buf.at(5, 1)
    end_loc = start_loc.end_of_line()

    gps_assert(
        buf.get_chars(start_loc, end_loc).rstrip(),
        EXPECTED,
        "Wrong autocasing after inserting newline in the middle of a line",
    )
