"""
Verifying that Show only errors preference is applied on the fly
"""

import GPS
from gs_utils.internal.utils import *

@run_test_driver
def test_driver():
    GPS.execute_action("Build All")
    yield wait_tasks()
    gps_assert(dump_locations_tree(),
               ['Builder results (3 items in 2 files)',
                ['main.adb (1 item)',
                ['<b>21:4</b>      warning: variable &quot;ObjA&quot;'
                 + ' is read but never assigned [-gnatwv]'],
                'a.adb (2 items)',
                ['<b>13:6</b>      error: statement expected',
                 '<b>28:3</b>      error: statement expected']]],
               "Unexpected contents of the Locations view")

    GPS.Preference ("locations-only-high-messages").set("True")
    yield wait_idle()
    gps_assert(dump_locations_tree(),
               ['Builder results (2 of 3 items in 1 file)',
                ['a.adb (2 items)',
                 ['<b>13:6</b>      error: statement expected',
                  '<b>28:3</b>      error: statement expected']]],
               "Unexpected contents of the Locations view")
