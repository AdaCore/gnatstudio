"""
Verify that goto type of entity works fine with the LSP.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    file = GPS.File("foo.adb")
    buf = GPS.EditorBuffer.get(file)
    buf.current_view().goto(buf.at(6, 14))

    GPS.execute_action("goto type of entity")
    yield hook("language_server_response_processed")

    current_buffer = GPS.EditorBuffer.get()
    current_loc = current_buffer.current_view().cursor()

    gps_assert(
        current_buffer.file(), GPS.File("foo.ads"),
        "goto type of entity  did not open the right file")
    gps_assert(
        current_loc.line(), 3, "Wrong line after Go To Declaration")
    gps_assert(
        current_loc.column(), 4, "Wrong column after Go To Declaration")
