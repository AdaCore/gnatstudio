""" Verify behaviour of block uncomment """
from gs_utils.internal.utils import run_test_driver, gps_assert


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    v = b.current_view()

    b.select(b.at(5, 8), b.at(9, 8))
    GPS.execute_action("uncomment lines")

    # Verify the uncommenting of a block that has both commented
    # and uncommented lines
    gps_assert(b.get_chars(b.at(5, 1), b.at(9, 14)),
               """   if True then
   a comment
   another comment
      if True then
         null;""",
               "Error when uncommenting a mixed block")

    b.select(b.at(11, 15), b.at(12, 14))
    GPS.execute_action("uncomment lines")

    # Verify the uncommenting of a block that doesn't use
    # two spaces after the comment marker
    gps_assert(b.get_chars(b.at(11, 1), b.at(12, 15)),
               """       a comment
      a comment""",
               "Error when uncommenting uneven block starts")
