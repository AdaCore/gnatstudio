"""
This test verifies that 'Expand alias under cursor' action does not
hang GS and does not change initial code when the cursor is at the
beginning of the file.
"""

from gs_utils.internal.utils import run_test_driver, gps_assert, wait_tasks, wait_idle

expected = """-- comment
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
procedure hello is
   X : Unbounded_String;

begin
   Append (X, "hello");
end;
"""


@run_test_driver
def driver():
    yield wait_tasks()
    b = GS.EditorBuffer.get(GS.File("main.adb"))
    GPS.execute_action("Expand alias under cursor")
    yield wait_idle()

    gps_assert(b.get_chars(), expected, "Autofix failed")
