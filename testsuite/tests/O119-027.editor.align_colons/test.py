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
    b = GS.EditorBuffer.get(GS.File("align.adb"))
    b.select()
    GS.execute_action("Align colons")
    gps_assert(b.get_chars(), expected, "Align colons failed")
