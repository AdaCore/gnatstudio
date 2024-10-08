"""
Test the autocasing while inserting characters and especially a new line.
"""
from gs_utils.internal.utils import *

EXPECTED = "Hello_World is Natural A and then B"


@run_test_driver
def driver():
    GPS.Preference("Ada-Casing-Policy").set("On_The_Fly")
    # Disable the completion thus wait_idle() will not freeze GPS
    GPS.Preference("Smart-Completion-Mode").set("Disabled")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    for ch in EXPECTED.lower():
        send_key_event(ord(ch))
        yield wait_idle()
    gps_assert(buf.get_chars().rstrip(), EXPECTED, "Wrong autocasing")
    buf.current_view().goto(buf.at(1, len(EXPECTED) - 1))
    send_key_event(GDK_RETURN)
    yield wait_idle()
    gps_assert(
        buf.get_chars().rstrip(),
        EXPECTED[:-2] + "\n    " + EXPECTED[-2:],
        "Issue when inserting new line",
    )
