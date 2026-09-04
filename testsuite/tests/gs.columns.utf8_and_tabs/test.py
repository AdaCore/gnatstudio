"""
Column positions are counted in characters, not in UTF-8 bytes, and the
editor API speaks visible columns, so a tab counts for its expanded width.
Checks extraction, editing and end-of-line positions on such lines.
"""

import GPS
from gs_utils.internal.utils import *

#  Line 2 is "   --  eee cafe end" with e-acute in place of the four 'e's
#  written here, so it holds 19 characters in 23 bytes.
UTF8_LINE = "   --  ééé café end"

TAB_LINE = "\tX := 1;"


@run_test_driver
def test_driver():
    GPS.Preference("General-Charset").set("UTF-8")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    ############################
    #  UTF-8: characters, not bytes

    gps_assert(
        buf.get_chars(buf.at(2, 1), buf.at(2, 1).end_of_line()),
        UTF8_LINE + "\n",
        "wrong contents of the UTF-8 line",
    )
    gps_assert(
        buf.at(2, 1).end_of_line().column(),
        len(UTF8_LINE) + 1,
        "wrong end-of-line column on the UTF-8 line",
    )

    #  A slice starting past the multi-byte characters
    gps_assert(
        buf.get_chars(buf.at(2, 12), buf.at(2, 15)),
        "café",
        "wrong slice after the multi-byte characters",
    )
    gps_assert(buf.at(2, 12).column(), 12, "wrong column round trip")

    #  Editing at a column past the multi-byte characters
    buf.delete(buf.at(2, 12), buf.at(2, 15))
    gps_assert(
        buf.get_chars(buf.at(2, 1), buf.at(2, 1).end_of_line()),
        "   --  ééé  end\n",
        "deleting after the multi-byte characters removed the wrong " "text",
    )

    ############################
    #  Tabs: a tab spans its expanded width in visible columns

    Tab_Width = GPS.Preference("Ada-Indent-Level").get()

    gps_assert(
        buf.get_chars(buf.at(4, 1), buf.at(4, 1).end_of_line()),
        TAB_LINE + "\n",
        "wrong contents of the line holding a tab",
    )
    gps_assert(
        buf.at(4, 1).end_of_line().column(),
        Tab_Width + 8,
        "the tab was not expanded in the end-of-line column",
    )

    #  The first visible column is the tab itself; the character after it
    #  starts at the first column past the expanded tab.
    gps_assert(
        buf.get_chars(buf.at(4, 1), buf.at(4, 1)),
        "\t",
        "visible column 1 of the tab line is not the tab",
    )
    gps_assert(
        buf.get_chars(buf.at(4, Tab_Width + 1), buf.at(4, Tab_Width + 1)),
        "X",
        "wrong character just after the expanded tab",
    )

    #  Collapsing a visible column and expanding it back agree: any visible
    #  column inside the expanded tab designates the character after it.
    gps_assert(
        buf.at(4, 2).column(),
        Tab_Width + 1,
        "collapsing and expanding a column across a tab disagree",
    )

    #  Deleting the tab leaves the rest of the line untouched
    buf.delete(buf.at(4, 1), buf.at(4, 1))
    gps_assert(
        buf.get_chars(buf.at(4, 1), buf.at(4, 1).end_of_line()),
        "X := 1;\n",
        "deleting the tab removed the wrong text",
    )
