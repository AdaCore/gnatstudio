"""
The obsolescent GPS.Editor shell commands take a column of 0, and let it be
omitted, to mean that no column is given. None of those must build an out of
range position, and each has a documented meaning for the absent column.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.Editor.edit("main.adb")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    ############################
    #  get_chars: no column means the whole line

    gps_assert(
        GPS.Editor.get_chars("main.adb", 2),
        "   X : Integer := 0;\n",
        "get_chars with the default column",
    )
    gps_assert(
        GPS.Editor.get_chars("main.adb", 2, 0),
        "   X : Integer := 0;\n",
        "get_chars with an explicit column of 0",
    )
    gps_assert(
        GPS.Editor.get_chars("main.adb", 2, 4),
        "   X : Integer := 0;\n",
        "get_chars with a real column",
    )

    ############################
    #  cursor_set_position: no column means the first non-blank character

    GPS.Editor.cursor_set_position("main.adb", 4)
    Cursor = buf.current_view().cursor()
    gps_assert(
        (Cursor.line(), Cursor.column()),
        (4, 4),
        "cursor_set_position with no column should go to the first "
        "non-blank character, it went to %s" % Cursor,
    )

    ############################
    #  select_text: a start column of 0 designates no position, so nothing
    #  gets selected; an end column of 0 means the whole line.

    buf.select(buf.at(2, 1), buf.at(2, 3))
    GPS.Editor.select_text(first_line=4, last_line=4, start_column=0, end_column=0)
    gps_assert(
        buf.selection_start().line(),
        buf.selection_end().line(),
        "select_text with a start column of 0 selected something",
    )

    GPS.Editor.select_text(first_line=4, last_line=4, start_column=4, end_column=5)
    gps_assert(
        buf.get_chars(buf.selection_start(), buf.selection_end().forward_char(-1)),
        "X",
        "select_text did not select a single character",
    )

    GPS.Editor.select_text(first_line=4, last_line=4, start_column=4)
    gps_assert(
        buf.get_chars(buf.selection_start(), buf.selection_end().forward_char(-1)),
        "X := 1;\n",
        "select_text with an end column of 0 should select the rest " "of the line",
    )

    ############################
    #  replace_text: a column of 0 replaces from the start of the line

    GPS.Editor.replace_text("main.adb", 2, 0, "--", 0, 2)
    gps_assert(
        GPS.Editor.get_chars("main.adb", 2),
        "-- X : Integer := 0;\n",
        "replace_text with a column of 0",
    )
