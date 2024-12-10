"""
This test checks that we take in account `config` switch
and pass it to ALS to have proper navigation

"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    yield wait_idle()
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.current_view().goto(b.at(5, 49))
    GPS.execute_action("goto declaration")
    yield hook("language_server_response_processed")
    yield wait_idle()

    current_buf = GPS.EditorBuffer.get()
    gps_assert(
        "sjlj" in current_buf.file().path,
        True,
        "Wrong RTS file opened " + current_buf.file().path,
    )
