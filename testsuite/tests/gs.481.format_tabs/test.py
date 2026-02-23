"""
Test block formatting using the LSP formatting without using selection in
a file containing tabs.
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_idle,
    gps_assert,
    wait_language_server,
)

expected_tabs = """	function F2 return Boolean
	is (F (A => True, B => False));

"""

expected_spaces = """	function F2 return Boolean
   is (F (A => True, B => False));

"""


@run_test_driver
def driver():
    GPS.Preference("Ada-Use-Tabs").set(True)
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))
    yield wait_idle()

    b.current_view().goto(b.at(5, 16))

    # Format the second line in the aggregate
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(
        b.get_chars(b.at(4, 1), b.at(6, 1)), expected_tabs, "Wrong formatting with tabs"
    )

    GPS.Preference("Ada-Use-Tabs").set(False)
    b.current_view().goto(b.at(5, 2))

    # Format again the aggregate by using spaces this time
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(
        b.get_chars(b.at(4, 1), b.at(6, 1)),
        expected_spaces,
        "Wrong formatting with spaces",
    )
