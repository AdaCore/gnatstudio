"""
Test width limit and GNATformat: both definitions are slitghly different.
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_idle,
    gps_assert,
    wait_language_server,
)

EXPECTED = """package body T is

   ----------------------
   -- Support_Language --
   ----------------------

   overriding
   function Support_Language
     (Self : access Outline_LSP_Provider; Lang : Language_Access)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Get_Language_Server (Lang) /= null;
   end Support_Language;

end T;
"""


@run_test_driver
def driver():
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    GPS.Preference("Src-Editor-Highlight-Column").set(80)
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))
    yield wait_idle()

    b.select()

    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(
        b.get_chars(),
        EXPECTED,
        "Wrong max line",
    )
