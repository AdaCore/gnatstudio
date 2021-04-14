"""
Verify the Call Graph Tree content for dispatching calls.
"""

import GPS
from gs_utils.internal.utils import *

call_expected = ['foo.adb:3:11',
                 ['pack.ads:11:25',
                  'pack.ads:7:14',
                  'pack.ads:10:14',
                  'pack.ads:6:14']]
loc_expected_a = ['    foo.adb']
loc_expected_b = ['    foo.adb (through dispatching)']


@run_test_driver
def driver():
    # Go to the entity
    b = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    b.current_view().goto(b.at(3, 13))
    yield wait_idle()

    # Execute "Foo calls"
    GPS.execute_action("Entity calls")
    yield wait_language_server("callHierarchy/outgoingCalls", "Ada")
    # Need to wait a little for the "Call Graph Location Tree" to be filled
    yield timeout(300)

    # Verify the general list and the first 2 rows, one of them is dispatching
    call_tree = get_widget_by_name("Call Graph Tree")
    gps_assert(dump_tree_model(call_tree.get_model(), 1),
               call_expected,
               "Wrong content for the Calls list")
    loc_tree = get_widget_by_name("Call Graph Location Tree")
    loc_model = loc_tree.get_model()
    click_in_tree(call_tree, Gtk.TreePath("0:0"))
    gps_assert(dump_tree_model(loc_model, 3),
               loc_expected_a,
               "Should not be dispatching")
    click_in_tree(call_tree, Gtk.TreePath("0:1"))
    gps_assert(dump_tree_model(loc_model, 3),
               loc_expected_b,
               "Should be dispatching")
