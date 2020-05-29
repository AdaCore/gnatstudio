"""
This test verifies that subprogram aspects are well formatted
"""
import GPS
import os.path
from gs_utils.internal.utils import *

Expected = """procedure Main is

   procedure Enter_Root_Mode
     with
       Global  => (Input  => CPU_Global.CPU_ID, In_Out => X86_64.State),
       Depends => (X86_64.State =>+ CPU_Global.CPU_ID);

   procedure Enter_Root_Mode with
     Global  => (Input  => CPU_Global.CPU_ID, In_Out => X86_64.State),
     Depends => (X86_64.State =>+ CPU_Global.CPU_ID);

begin
   null;
end Main;
"""


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    buf.current_view().goto(buf.at(8, 71))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    buf.current_view().goto(buf.at(5, 73))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    gps_assert(buf.get_chars(),
               Expected,
               "Formatting is incorrect")
