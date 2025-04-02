"""
This test checks that `Delete TAB with spaces` actions works.
"""

from time import time
import GPS
from gs_utils.internal.utils import *

expected = """
with Ada.Text_IO;  use Ada.Text_IO;

procedure Main is
begin
   Put_Line (".");
   Put_Line (".");
   Put_Line (".");
   
end Main;
"""


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(9, 7))
    # Delete one indentation (spaces or tab)  backward
    GPS.execute_action("Delete TAB with spaces")
    yield wait_idle()

    gps_assert(buf.get_chars(), expected, "Incorrect deletion of spaces")
    
    # Tests <tab>
    GPS.Preference("Ada-Use-Tabs").set(True)
    yield wait_idle()
    buf.current_view().goto(buf.at(7, 1).end_of_line())
    yield wait_idle()
    send_key_event(GDK_RETURN)
    yield wait_idle()
    GPS.execute_action("Delete TAB with spaces")
    gps_assert(
        buf.get_chars(buf.at(8, 1), buf.at(8, 1).end_of_line()),
        "\n",
        "Incorrect deletion of tab character",
    )    
