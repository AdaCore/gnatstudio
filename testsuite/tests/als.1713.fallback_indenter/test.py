"""
Test fallback indenter for rangeFormatting request from the ALS.
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_idle,
    gps_assert,
    wait_language_server,
)

ERROR = "GNATformat: Syntactically invalid code can't be formatted"

EXPECTED_1 = """   X : Integer := 1;
   YYY : Natural := 2;
begin
   if X = YYY
     or else X + X > YYY
     and then (X < YYY
           and then Y > 2)
   then
      null;
   end if;
"""

EXPECTED_2 = """   for I in 1 .. X loop
      if X > 2 then
         Y := Y + 1;
      end if;
   end loop;
"""

EXPECTED_3 = """   case X is
   when X > 10 =>
      null;
   when X > 20 =>
      null;
   when X > 30
      | Y > 10 =>
      null;
   when others =>
      null;
   end case;"""


def test_range(buf, start_loc, end_loc, expected, msg):
    buf.select(start_loc, end_loc)
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()
    gps_assert(
        b.get_chars(start_loc, end_loc), expected, "Wrong indentation for " + msg
    )


@run_test_driver
def driver():
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    GPS.Preference("LSP-Ada-Formatting-Fallback").set(False)
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))
    yield wait_idle()

    b.select(b.at(2, 0), b.at(11, 0).end_of_line())
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(
        ERROR in GPS.Console("Messages").get_text(), True, "Missing error message"
    )

    # Activate
    GPS.Preference("LSP-Ada-Formatting-Fallback").set(True)

    test_range(b, b.at(2, 0), b.at(11, 0), EXPECTED_1, "if condition")
    test_range(b, b.at(13, 0), b.at(17, 0), EXPECTED_2, "for loop")
    test_range(b, b.at(19, 0), b.at(29, 0), EXPECTED_3, "when case")
