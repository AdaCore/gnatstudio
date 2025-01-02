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
    yield wait_idle()
    GPS.execute_action("goto declaration")
    yield wait_language_server("textDocument/declaration")
    yield wait_idle()

    current_buf = GPS.EditorBuffer.get()
    gps_assert(
        "light" in current_buf.file().path,
        True,
        "Wrong RTS file opened " + current_buf.file().path,
    )
