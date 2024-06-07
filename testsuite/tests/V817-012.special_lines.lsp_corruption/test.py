"""
Check that there are no corruption after deleting a line containing a mark
related to a "special line"
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    global COUNT_DELETE
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    # The goal here is to monitor the LSP's listener to Buffer_Delete_Range
    # and how often it is called: to do so, delete a line and verify
    # that a basic LSP action still work.

    def test_goto(msg):
        buf.current_view().goto(buf.at(5, 5))
        GPS.execute_action("goto declaration")
        yield wait_language_server("textDocument/declaration")
        current_loc = buf.main_cursor().location()
        gps_assert(current_loc, buf.at(2, 15), "Goto Failed when %s" % msg)

    # Delete \n without any mark on it
    buf.delete(buf.at(2, 1).end_of_line(), buf.at(2, 1).end_of_line())
    yield wait_idle()
    yield test_goto("deleting a simple new line")

    # Reset the buffer
    buf.undo()
    yield wait_idle()

    # Add special line and delete the line before which is containing the
    # mark of the special line thus removing it
    m = buf.add_special_line(3, "Special lines")
    buf.delete(buf.at(2, 1).end_of_line(), buf.at(2, 1).end_of_line())
    yield wait_idle()
    yield test_goto("deleting a new line with a special line's mark")
