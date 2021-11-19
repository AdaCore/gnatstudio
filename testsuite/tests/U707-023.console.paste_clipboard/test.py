"""
This test verifies "Paste to console" action.
"""

from gs_utils.internal.utils import *


@run_test_driver
def driver():
    GPS.execute_action("new file")
    buf = GPS.EditorBuffer.get()
    buf.insert(buf.beginning_of_buffer(), "echo Test > aaa.adb")
    buf.copy()
    GPS.execute_action("open os shell")
    GPS.execute_action("Paste to console")
    send_key_event(GDK_RETURN)
    buf.close(force=True)
    yield wait_idle()
    yield timeout(500)
    aaa = GPS.EditorBuffer.get(GPS.File("aaa.adb"))
    text = aaa.get_chars()
    gps_assert(text, "Test\n")
