from gs_utils.internal.utils import run_test_driver, gps_assert

expected = """project Test is

   for Source_Dirs use ("."); -- Source
   for Object_Dir use "obj";  -- Object
   for Main use ("main.adb"); -- Main

end Test;
"""


@run_test_driver
def driver():
    b = GS.EditorBuffer.get(GS.File("test.gpr"))
    b.select()
    GS.execute_action("Align end of line comments")
    gps_assert(b.get_chars(), expected, "Align comments failed")
