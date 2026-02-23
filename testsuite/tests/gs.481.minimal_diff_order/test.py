"""
Test block formatting using the LSP formatting without using selection.
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_idle,
    gps_assert,
    wait_language_server,
)

expected = """   function F2 return Boolean is
      (F (A => True,
 """


@run_test_driver
def driver():
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))
    yield wait_idle()

    b.current_view().goto(b.at(5, 16))

    # Retrieve the character next to the cursor's location
    cursor_loc = b.main_cursor().location()
    original_cursor_char = b.get_chars(cursor_loc, cursor_loc.forward_word())

    # Format the second line in the aggregate
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(b.get_chars(b.at(4, 1), b.at(6, 1)), expected, "Wrong formatting")

    # Verify that the cursor is still on the same character, despite
    # that lines have been rearranged when formatting
    cursor_loc = b.main_cursor().location()
    new_cursor_char = b.get_chars(cursor_loc, cursor_loc.forward_word())
    gps_assert(
        original_cursor_char,
        new_cursor_char,
        "The character pointed by the cursor has changed after formatting",
    )
