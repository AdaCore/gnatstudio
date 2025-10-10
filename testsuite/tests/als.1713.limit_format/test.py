"""
Verify that Limit-LSP-Formatting works properly with selection.
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_idle,
    gps_assert,
    wait_language_server,
)


EXPECTED_1 = """procedure T is
   function F1 (A : Boolean; B : Boolean) return Boolean is (True);

   function F2 (A : Boolean; B : Boolean) return Boolean is (False);

   function F3 return Boolean
   is (F (A => True, B => False));

   function F4 return Boolean is (F2 (A => True, B => False));
begin
   null;
end T;
"""

EXPECTED_2 = """procedure T is
   function F1 (A : Boolean; B : Boolean) return Boolean
   is (True);

   function F2 (A : Boolean; B : Boolean) return Boolean
   is (False);

   function F3 return Boolean
   is (F (A => True, B => False));

   function F4 return Boolean is (F2 (A => True, B => False));
begin
   null;
end T;
"""

EXPECTED_3 = """procedure T is
   function F1 (A : Boolean; B : Boolean) return Boolean
   is (True);

   function F2 (A : Boolean; B : Boolean) return Boolean
   is (False);

   function F3 return Boolean
   is (F (A => True, B => False));

   function F4 return Boolean
   is (F2 (A => True, B => False));
begin
   null;
end T;
"""


@run_test_driver
def driver():
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    GPS.Preference("Limit-LSP-Formatting-ada").set(True)
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))
    yield wait_idle()

    b.current_view().goto(b.at(6, 4))
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(b.get_chars(), EXPECTED_1, "Wrong formatting for case 1")

    b.select(b.at(2, 4), b.at(6, 4))
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(b.get_chars(), EXPECTED_2, "Wrong formatting for case 2")

    b.select()
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(b.get_chars(), EXPECTED_3, "Wrong formatting for case 3")
