"""
Verify that GS does not apply the second formatting response 
on the already modified file
"""
import GPS
from gs_utils.internal.utils import *

expected = """with Ada.Text_IO;

procedure Main is
begin
   Ada.Text_IO.Put_Line ("Hello");
end Main;
"""


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()
    GPS.execute_action("autoindent file")
    GPS.execute_action("autoindent file")
    yield wait_language_server("textDocument/formatting", "ada")
    yield wait_idle()

    gps_assert(buf.get_chars(include_hidden_chars=False),
               expected,
               "Formatting does not work properly")
