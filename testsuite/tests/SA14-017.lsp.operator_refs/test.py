"""
Test for "find all references" with operators
"""
import GPS
from gs_utils.internal.utils import *


expected_msgs = [
    'References for / (main.adb:6) (2 items in 1 file)',
    ['main.adb (2 items)',
     ['<b>6:13</b>      function ' +
      '<b>&quot;/&quot;</b> (Left, Right : My_Int) return My_Int',
      '<b>11:25</b>     Result : My_Int := A <b>/</b> B;']]]


@run_test_driver
def driver():
    main = GPS.EditorBuffer.get(GPS.File("main.adb"))
    main.current_view().goto(main.at(6, 14))
    GPS.execute_action("find all references")
    yield hook("language_server_response_processed")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(dump_locations_tree(), expected_msgs,
               "'find all references' does not display operators " +
               "properly")
