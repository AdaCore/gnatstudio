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

expected2 = """with Ada.Text_IO;

procedure Main is
begin
Ada.Text_IO.Put_Line ("Hello");
end Main;
"""


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()

    # Verify that launching autoindent twice does not modify the file twice
    GPS.execute_action("autoindent file")
    GPS.execute_action("autoindent file")
    yield wait_language_server("textDocument/formatting", "ada")
    yield wait_idle()

    gps_assert(
        buf.get_chars(include_hidden_chars=False),
        expected,
        "Formatting does not work properly",
    )

    # Verify that formatting is properly intercepted if an edit is made before
    # the response is received
    GPS.execute_action("autoindent file")

    # Delete the spaces before "Ada.Text_IO" on line 5
    buf.delete(buf.at(5, 1), buf.at(5, 3))

    # Wait for the language server to reply with the formatting
    yield wait_language_server("textDocument/formatting", "ada")
    yield wait_idle()

    # Verify that the formatting is not applied
    gps_assert(
        buf.get_chars(include_hidden_chars=False),
        expected2,
        "Formatting was applied even if an edit was done after requesting it",
    )
