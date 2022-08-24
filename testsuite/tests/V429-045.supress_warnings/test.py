"""
Check if supress warnings works
"""

from GPS import *
from gs_utils.internal.utils import *
import traceback

expected = """with Ada.Text_IO;

procedure Main is
   pragma Warnings
     (Off,
      Reason => "variable ""B"" is read but never assigned [-gnatwv]");
   --  TODO: Add explanations
   B : Boolean;
   pragma Warnings (On);
begin
   Ada.Text_IO.Put_Line ("Hello" & B'Img);
end Main;
"""


@run_test_driver
def run_test():
    ed = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.BuildTarget("Build All").execute(force=True)
    yield wait_tasks()

    ed.click_on_side_column(line=4, column=1,
                            icon_name="gps-codefix")
    gps_assert(ed.get_chars(), expected)

    GPS.BuildTarget("Build All").execute(force=True)
    yield wait_tasks()
    gps_assert(dump_locations_tree(), [])
