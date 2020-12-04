"""
This test checks that executing navigation actions on mixed-cased identifiers
mentioned in Ada comments works fine if this identifier is referenced in the
current file.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(3, 32))

    GPS.execute_action("goto declaration or body")
    yield wait_language_server("textDocument/definition", "Ada")

    current_buf = GPS.EditorBuffer.get()
    current_loc = current_buf.current_view().cursor()

    gps_assert(current_buf.file(), GPS.File("test.ads"), "Wrong file opened")
    gps_assert(current_loc.line(), 3, "Wrong location after 'goto declaration'")
