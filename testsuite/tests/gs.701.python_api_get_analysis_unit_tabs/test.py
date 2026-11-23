"""
Test GPS.EditorBuffer.get_analysis_unit with tabs
"""

import libadalang as lal
from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    ed = GPS.EditorBuffer.get(GPS.File("main.adb"))
    unit = ed.get_analysis_unit()
    # The line starts with "\tfunction" with Size_Tabs=3 by default
    node = unit.root.lookup(lal.Sloc(2, 5))
    gps_assert(str(node.text), "function", "Wrong node selected")
    gps_assert(str(node.sloc_range), "2:4-2:12", "Issue with tabs")

    GPS.Preference("Ada-Indent-Level").set(6)
    # Need to re-get the analysis unit because the context was changed
    unit = ed.get_analysis_unit()
    # Use (2, 7) compare to (2, 5) to still get the "function" node
    node = unit.root.lookup(lal.Sloc(2, 7))
    gps_assert(str(node.text), "function", "Wrong node selected")
    gps_assert(str(node.sloc_range), "2:7-2:15", "Issue with tabs")
