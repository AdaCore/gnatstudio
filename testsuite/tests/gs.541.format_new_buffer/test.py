"""
Create a new buffer and try to format it
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_idle,
    gps_assert,
    wait_language_server,
    timeout,
)

INIT_TEXT = """procedure Main is
begin
null;
end Main;"""

EXPECTED_TEXT = """procedure Main is
begin
   null;
end Main;"""


@run_test_driver
def driver():
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()
    b.insert(b.at(1, 1), INIT_TEXT)
    b.select(b.at(0, 0), b.at(4, 9))
    yield timeout(5000)

    # Format the second line in the aggregate
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(
        b.get_chars(), EXPECTED_TEXT, "Wrong formatting"
    )
