"""
Verify that the outline properly display Unchecked_Free implementation.
"""

from GPS import *
from gs_utils.internal.utils import *


expected = ["P", ["T", "T_Access", "Free"]]


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("p.ads"))
    GPS.execute_action("open Outline")
    yield wait_outline("p.ads")

    explorer = get_widget_by_name("Outline View Tree")
    gps_assert(
        dump_tree_model(explorer.get_model(), 1), expected, "Wrong outline content"
    )
