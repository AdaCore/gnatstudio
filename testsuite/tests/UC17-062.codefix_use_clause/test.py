"""
This test checks that code-fix set use clause correctly.
"""
from gs_utils.internal.utils import *

expected = """
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;
use Ada.Strings.Wide_Wide_Unbounded;
with Types;       use Types;

with A;
with B;

procedure Main is
   My_Text, Your_Text : Ada.Strings.Wide_Wide_Unbounded.
     Unbounded_Wide_Wide_String;

   V_A : A_Type;
   V_B : B_Type;

   AA, BB : Rec;
begin
   if My_Text = Your_Text then
      Put_Line ("Equal");
   end if;

   Put_Line (Boolean'Image (A."=" (V_A, V_B)));
   Put_Line (Boolean'Image (A."=" (AA.A, BB.B)));
end;
"""


@run_test_driver
def driver():
    b = GS.EditorBuffer.get(GS.File("main.adb"))
    GPS.BuildTarget("Build All").execute(force=True)
    yield wait_tasks()

    b.click_on_side_column(line=23, column=1,
                           icon_name="gps-codefix")
    b.click_on_side_column(line=22, column=1,
                           icon_name="gps-codefix")
    b.click_on_side_column(line=18, column=1,
                           icon_name="gps-codefix")
    gps_assert(b.get_chars(), expected)
