"""
Tests that `Delete word backward` action deletes empty line
"""

import GPS
from gs_utils.internal.utils import *

before = """procedure Main is
   X : Integer;
begin
   null;
   
end Main;
"""

after = """procedure Main is
   X : Integer;
begin
   null;

end Main;
"""

after1 = """procedure Main is
   X : Integer;
begin
   null;
end Main;
"""


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()
    b.current_view().goto(b.at(4, 9))
    
    # Add new line with the ENTER key. This will add 3 extra spaces
    # due to auto-indentation.
    send_key_event(GDK_RETURN)
    yield wait_idle()
    gps_assert(
        b.get_chars(b.at(1, 1), b.at(6, 1).end_of_line()),
        before,
        "Incorrect enter",
    )

    # Delete spaces in the new line, so cursor will be at the
    # beggining of the line
    GPS.execute_action("Delete word backward")
    yield wait_idle()
    gps_assert(
        b.get_chars(b.at(1, 1), b.at(6, 1).end_of_line()),
        after,
        "Incorrect deleting",
    )

    # Delete the new empty line (because we are at the beggining of the line)
    GPS.execute_action("Delete word backward")
    yield wait_idle()
    gps_assert(
        b.get_chars(b.at(1, 1), b.at(5, 1).end_of_line()),
        after1,
        "Incorrect deleting empty line",
    )
    
    # Use <tab> for indentation and test that we delete them properly
    GPS.Preference("Ada-Use-Tabs").set(True)
    yield wait_idle()
    b.current_view().goto(b.at(4, 9))
    yield wait_idle()
    send_key_event(GDK_RETURN)
    yield wait_idle()

    GPS.execute_action("Delete word backward")
    yield wait_idle()
    gps_assert(
        b.get_chars(b.at(5, 1), b.at(5, 1).end_of_line()),
        "\n",
        "Incorrect deleting tabulator",
    )
