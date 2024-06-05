"""
Test text_utils.strip_trailing_blanks
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils

unstripped = (
    "procedure Foo is       -- Hello\n"
    + "begin   \n"
    + "   null;      \n"
    + "end Foo;       \n"
)

expected = """procedure Foo is       -- Hello
begin
   null;
end Foo;
"""


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.get_chars(), unstripped, "should have blanks")
    text_utils.strip_trailing_blanks()
    gps_assert(buf.get_chars(), expected, "should have strip blanks")
