"""
Test indentOnly preference
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_1 = """procedure T is
   function F1 (A : Boolean; B : Boolean) return Boolean
   is
     (True);

   function F2 (A : Boolean; B : Boolean) return Boolean is
     (False);
begin
   null;
end T;
"""

EXPECTED_2 = """procedure T is
   function F1 (A : Boolean; B : Boolean) return Boolean
   is
     (True);

   function F2 (A : Boolean; B : Boolean) return Boolean
   is (False);
begin
   null;
end T;
"""


@run_test_driver
def driver():
    GPS.Preference("Editor-On-Type-Formatter-ada").set("LSP")
    GPS.Preference("LSP-Ada-On-Type-Formatting").set("indent")
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))
    yield wait_idle()

    b.current_view().goto(b.at(2, 58))
    send_key_event(GDK_RETURN)
    yield wait_language_server("textDocument/onTypeFormatting")
    yield wait_idle()

    gps_assert(b.get_chars(), EXPECTED_1, "Wrong formatting for indentOnly")

    GPS.Preference("LSP-Ada-On-Type-Formatting").set("format")
    b.current_view().goto(b.at(6, 58))
    send_key_event(GDK_RETURN)
    yield wait_language_server("textDocument/onTypeFormatting")
    yield wait_idle()

    gps_assert(b.get_chars(), EXPECTED_2, "Wrong formatting without indentOnly")
