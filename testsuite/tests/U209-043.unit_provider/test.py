# Check that the unit provider does not generate an infinite
# recursion.

from gs_utils.internal.utils import run_test_driver, gps_assert
import lal_utils
import os


@run_test_driver
def driver():
    f = GPS.File("locpack.adb")
    b = GPS.EditorBuffer.get(f)
    node = lal_utils.node(f, 3, 7, 'SubpBody')

    # This is the call that used to cause an infinite recursion;
    # calling this will either crash GS or generate an exception
    # traceback
    decl = node.p_decl_part()

    # Check that the operation was successful
    gps_assert(os.path.basename(decl.unit.filename),
               "locpack.ads",
               "p_decl_part() didn't work")
