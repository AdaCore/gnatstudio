"""
This test checks that we highlight the editor's current selection when
opening the Search view, to help users visualizing which area of the code
they are looking to.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


EXPECTED_LINE_HIGHLIGHTING = ["", "Compiler info", "Compiler info", "", "", "", ""]

EXPECTED_NO_HIGHLIGHTING = ["", "", "", "", "", "", ""]

EXPECTED_SYNTAX_HIGHLIGHTING = """.................
.......#######......
.......#######......
.....
..............
........."""


@run_test_driver
def test_driver():
    # Disable this preference to avoid losing focus when clicking
    # on 'Find all'

    GPS.Preference("locations-auto-jump-to-first").set(False)

    # Select an area in the editor

    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    editor_view = buf.current_view()

    editor_view.goto(buf.at(1, 1).end_of_line())
    editor_view.goto(buf.at(4, 1), extend_selection=True)

    # Open the Search view and check that we have highlighted
    # the editor's selected area

    s = dialogs.Search()
    yield s.open_and_yield()
    yield wait_idle()

    gps_assert(
        buf.debug_dump_line_highlighting(),
        EXPECTED_LINE_HIGHLIGHTING,
        "The editor's selected area has not been highlighted when "
        + "opening the Search view",
    )

    # Search for 'Integer': verify that the search area is still highlighted
    # and that results are correctly highlighted too.

    s.pattern.set_text("Integer")
    yield s.yield_find_all()

    gps_assert(
        buf.debug_dump_line_highlighting(),
        EXPECTED_LINE_HIGHLIGHTING,
        "The editor's selected area should still be highlighted "
        + "since the Search view has still the focus",
    )

    gps_assert(
        buf.debug_dump_syntax_highlighting("Search results").strip(),
        EXPECTED_SYNTAX_HIGHLIGHTING.strip(),
        "'Find all' results are not correctly highlighted",
    )

    # Open another view to remove the focus from the Search view:
    # the editor's selected area should not be highlighted anymore

    GPS.execute_action("open Files")
    gps_assert(
        buf.debug_dump_line_highlighting(),
        EXPECTED_NO_HIGHLIGHTING,
        "The editor's selected area should not be highlighted "
        + "anymore once the Search view loses the focus",
    )
