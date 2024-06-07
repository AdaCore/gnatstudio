"""
Verify the Call Graph Tree content when called on spec: the location
should be corrected via preparecallHierarchy.
"""

import GPS
from gs_utils.internal.utils import *

call_expected = ["p.ads:3:14", ["p.ads:4:14"]]
loc_expected = ["    p.adb"]


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("p.ads"))
    b.current_view().goto(b.at(3, 15))

    GPS.execute_action("Entity calls")
    yield wait_language_server("textDocument/prepareCallHierarchy", "Ada")
    yield wait_language_server("callHierarchy/outgoingCalls", "Ada")
    # Wait for the tree to be filled
    yield timeout(300)

    call_tree = get_widget_by_name("Call Graph Tree")
    gps_assert(
        dump_tree_model(call_tree.get_model(), 1),
        call_expected,
        "Wrong content for the Calls list",
    )
    click_in_tree(call_tree, path="0:0")

    loc_tree = get_widget_by_name("Call Graph Location Tree")
    loc_model = loc_tree.get_model()
    gps_assert(
        dump_tree_model(loc_model, 3), loc_expected, "Wrong location for the call"
    )
