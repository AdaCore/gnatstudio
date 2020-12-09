"""
Test text_utils.untabify
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils

expected_3 = """procedure Foo is
begin
   null;
end Foo;
"""

expected_6 = """procedure Foo is
begin
      null;
end Foo;
"""


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)
    text_utils.untabify()
    gps_assert(buf.get_chars(), expected_3, "default pref value fails")
    buf.undo()
    GPS.Preference("Ada-Indent-Level").set(6)
    text_utils.untabify()
    gps_assert(buf.get_chars(), expected_6, "tab = 6 fails")
