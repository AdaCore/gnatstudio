"""
Verify that commands for handling string literals works.
"""

from gs_utils.internal.utils import *

expected = '''with ada.text_io;

procedure hello is
   V : String := "V";
   T : String := "li" & V & "ter"
     & "al";
begin
   if True then
      --  Comment
      --  c
      Ada.Text_IO.Put_Line ("hel" & V & "lo");
   end if;
end;
'''


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    v = b.current_view()

    # Split literal
    v.goto(b.at(9, 33))
    GPS.execute_action("Insert in string template")
    yield wait_idle()
    gps_assert(v.cursor(), GPS.EditorLocation(b, 9, 37))
    send_key_event(ord('V'))
    yield wait_idle()

    # Add comment
    v.goto(b.at(8, 18))
    GPS.execute_action("Wrap string/comment")
    send_key_event(ord("c"))
    yield wait_idle()

    # Split string literal into two lines
    v.goto(b.at(5, 24))
    GPS.execute_action("Wrap string/comment")
    yield wait_idle()
    gps_assert(v.cursor(), GPS.EditorLocation(b, 6, 9))

    # Paste clipboard
    b.select(b.at(4, 4), b.at(4, 5))
    GPS.execute_action("Copy to clipboard")
    v.goto(b.at(5, 21))
    GPS.execute_action("Paste into string literal")
    yield wait_idle()
    gps_assert(v.cursor(), GPS.EditorLocation(b, 5, 30))

    gps_assert(b.get_chars(), expected)
