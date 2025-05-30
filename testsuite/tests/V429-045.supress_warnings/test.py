"""
Check if the "suppress warnings" codefix action works
"""

from GPS import *
from gs_utils.internal.utils import *
import traceback

expected = """with Ada.Text_IO;

procedure Main is
   pragma Warnings (Off, "variable * is read but never assigned", Reason => "TBD");
   B :
     Boolean;
   pragma Warnings (On, "variable * is read but never assigned");
begin
   Ada.Text_IO.Put_Line ("Hello" & B'Img);
end Main;
"""

@run_test_driver
def run_test():
    ed = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.BuildTarget("Build All").execute(force=True)
    yield wait_tasks()

    ed.click_on_side_column(line=4, column=1, icon_name="gps-codefix")
    gps_assert(ed.get_chars(), expected)

    menu = get_widget_by_name("gnatstudio_code_actions_menu")
    gps_assert(menu is not None, False, "Menu should be empty")
