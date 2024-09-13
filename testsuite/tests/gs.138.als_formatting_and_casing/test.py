"""
Test combinaison of ALS formatting and autocasing settings.
"""

from GPS import *
from gs_utils.internal.utils import *


EXPECTED1 = """procedure File1 is
begin
null
end File1;
"""

EXPECTED2 = """procedure File2 is
begin
null;
end File2;
"""

EXPECTED3 = r"""procedure File3 is
begin
   null
   ;
end File3;
"""


@run_test_driver
def on_gps_started():
    Preference("Ada-Casing-Policy").set("On_The_Fly")
    buf1 = EditorBuffer.get(File("file1.adb"))

    buf1.insert(buf1.at(3, 3), "l")
    yield wait_idle()
    gps_assert(buf1.get_chars(), EXPECTED1, "Issue for casing on_the_fly")

    Preference("Ada-Casing-Policy").set("End_Of_Word")
    buf2 = EditorBuffer.get(File("file2.adb"))

    buf2.current_view().goto(buf2.at(3, 5))
    send_key_event(ord(";"))
    yield wait_idle()
    gps_assert(buf2.get_chars(), EXPECTED2, "Issue for casing end_of_word")

    Preference("Ada-Casing-Policy").set("End_Of_Line")
    buf3 = EditorBuffer.get(File("file3.adb"))

    buf3.current_view().goto(buf3.at(3, 5))
    send_key_event(GDK_RETURN)
    yield wait_idle()
    gps_assert(buf3.get_chars(), EXPECTED3, "Issue for casing end_of_line")
