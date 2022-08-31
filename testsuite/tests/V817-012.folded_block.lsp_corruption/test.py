"""
Check that there are no corruption after deleting a folded block.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    global COUNT_DELETE
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    safeguard = buf.get_chars()

    # The goal here is to monitor the LSP's listener to Buffer_Delete_Range
    # and how often it is called: to do so, delete lines and verify
    # that a basic LSP action still work.

    def test_goto(msg):
        buf.current_view().goto(buf.at(7, 5))
        GPS.execute_action('goto declaration')
        yield wait_language_server('textDocument/declaration')
        current_loc = buf.main_cursor().location()
        gps_assert(current_loc,
                   buf.at(3, 15),
                   "Goto Failed when %s" % msg)

    # Delete a new line
    buf.delete(buf.at(6, 1), buf.at(8, 1).end_of_line())
    yield wait_idle()
    yield test_goto("deleting a new_line in a folded block")

    # Reset the buffer
    buf.undo()
    yield wait_idle()

    yield wait_until_true(
        lambda: buf.has_blocks_information())
    buf.at(7, 1).block_fold()
    yield wait_idle()

    # This deletion will just unfold the block ...
    buf.delete(buf.at(6, 1), buf.at(8, 1).end_of_line())
    yield wait_idle()
    gps_assert(buf.get_chars(), safeguard, "First deletion should just unfold")
    # ... and this one will remove it
    buf.delete(buf.at(6, 1), buf.at(8, 1).end_of_line())

    yield test_goto("deleting a folded block")
