"""
Verify that rangeFormatting works fine on file with tabs. The second format
request is to verify that the buffer was not corrupted and it should not
change the buffer in GS.
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_idle,
    gps_assert,
    wait_language_server,
)


EXPECTED = """procedure T is
   function Foo (I : Integer) return Boolean
   is (True);

   XXXXXXXXXXXXXX : Integer;
   YYYYYYYYYYYYYY : Integer;
begin
   if Foo
        (XXXXXXXXXXXXXX + XXXXXXXXXXXXXX * YYYYYYYYYYYYYY
         = YYYYYYYYYYYYYY + YYYYYYYYYYYYYY)
   then
      null;
   end if;
end T;
"""


@run_test_driver
def driver():
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    GPS.Preference("Limit-LSP-Formatting-ada").set(False)
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))
    yield wait_idle()

    b.select()
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()
    gps_assert(b.get_chars(), EXPECTED, "Wrong formatting with tabs once")

    b.select()
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(b.get_chars(), EXPECTED, "Wrong formatting with tabs twice")
