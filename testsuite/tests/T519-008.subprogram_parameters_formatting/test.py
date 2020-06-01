"""
This test verifies that subprogram parameters are well formatted
"""
import GPS
import os.path
from gs_utils.internal.utils import *

Expected = """procedure Main is
begin
   Set_Num_Vector
     (D,
      Cps_Result_Name,
      Strat_Name,
      Name & "[" & Und & "]",
      Get_Num_Vector
        (Cps_Pricing_Dict,
         Book_Pricing_Result,
         Sub_Book_Name,
         Name & "_" & Und & "[" & Und & "]"));
end Main;
"""


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()

    buf.current_view().goto(buf.at(3, 18))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    buf.current_view().goto(buf.at(4, 9))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    buf.current_view().goto(buf.at(5, 23))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    buf.current_view().goto(buf.at(6, 18))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    buf.current_view().goto(buf.at(7, 30))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    buf.current_view().goto(buf.at(8, 21))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    buf.current_view().goto(buf.at(9, 27))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    buf.current_view().goto(buf.at(10, 30))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    buf.current_view().goto(buf.at(11, 24))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    gps_assert(buf.get_chars(),
               Expected,
               "Formatting is incorrect")
