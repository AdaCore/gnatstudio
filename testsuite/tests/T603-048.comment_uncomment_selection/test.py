"""Check the edge cases of selection when commenting/uncommenting:
do not consider the lines where the cursor is if it's not actually
selected"""
from gs_utils.internal.utils import run_test_driver, gps_assert


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("code.adb"))
    b.select(b.at(3, 1), b.at(5, 1))
    GPS.execute_action("toggle comment")
    gps_assert(b.get_chars(),
               """procedure code is
begin
   --  a;
   --  b;
   c;
end code;
""",
               "wrong lines taken into account when multi-line comment")

    b.select(b.at(3, 1), b.at(4, 1))
    GPS.execute_action("toggle comment")
    gps_assert(b.get_chars(),
               """procedure code is
begin
   a;
   --  b;
   c;
end code;
""",
               "wrong lines taken into account when multi-line uncomment")
