from gs_utils.internal.utils import run_test_driver, gps_assert

expected = """procedure Align is
   X     : Integer;
   Yyyyy : Integer;
begin
   null;
end Align;
"""


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("align.adb"))
    b.select()
    GPS.execute_action("Align colons")
    gps_assert(b.get_chars(),
               expected,
               "Align colons failed")
