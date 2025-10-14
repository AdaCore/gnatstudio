"""
Basic test for the computation of the minimal textEdits when formatting.
In particular we check that formatting has not changed the cursor's mark
(i.e: that it still points to the same character after formatting).
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_idle,
    gps_assert,
    wait_language_server,
)

expected = """procedure t is
   function F (A : Boolean; B : Boolean) return Boolean
   is (True);

   function F2 return Boolean
   is (F (A => True, B => False));
"""


@run_test_driver
def driver():
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    # Set the conditional continuation pref to a ridiculous amount
    GPS.Preference("Ada-Conditional-Level").set(120)
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))
    yield wait_idle()

    b.select(b.at(2, 1), b.at(6, 1).end_of_line())

    # Retrieve the character next to the cursor's location
    cursor_loc = b.main_cursor().location()
    original_cursor_char = b.get_chars(cursor_loc, cursor_loc.forward_word())

    # Format the second line in the aggregate
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    # Verify that the proper indentation is produced
    gps_assert(
        b.get_chars(b.at(1, 1), b.at(6, 1).end_of_line()),
        expected,
        "Wrong format for aggregate in expression function",
    )

    # Verify that the cursor is still on the same character, despite
    # that lines have been rearranged when formatting
    cursor_loc = b.main_cursor().location()
    new_cursor_char = b.get_chars(cursor_loc, cursor_loc.forward_word())
    gps_assert(
        original_cursor_char,
        new_cursor_char,
        "The character pointed by the cursor has changed after formatting",
    )
