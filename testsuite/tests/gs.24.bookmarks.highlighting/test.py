"""
This test checks that we correctly highlight the lines when
bookmarks are being set. It also checks that the bookmark
highlighting does not disappear when highlighting a part of
the line.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


EXPECTED_HIGHLIGHTING = ["", "Editor bookmarks", "", "", "", "", ""]


@run_test_driver
def test_driver():
    # Create a bookmark in main.adb and check that the line
    # is correctly highlighted.
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    editor_view = buf.current_view()
    editor_view.goto(buf.at(2, 4))
    GPS.execute_action("bookmark create")

    gps_assert(
        buf.debug_dump_line_highlighting(),
        EXPECTED_HIGHLIGHTING,
        "Line highlighting for bookmarks does not work",
    )

    # Search for 'Integer': this should highlight the search result
    # on the same line as the bookmark. Verify that the bookmark highlighting
    # is still present though.
    s = dialogs.Search()
    yield s.open_and_yield()
    yield wait_idle()
    s.pattern.set_text("Integer")
    yield s.yield_find_all()

    gps_assert(
        buf.debug_dump_line_highlighting(),
        EXPECTED_HIGHLIGHTING,
        "Line highlighting for bookmarks has been removed after searching",
    )
