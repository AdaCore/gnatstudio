"""
Check if GPS.EditorBuffer.get_analysis_unit work
"""

from GPS import *
from gs_utils.internal.utils import *
import traceback


@run_test_driver
def run_test():
    ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
    unit = ed.get_analysis_unit()
    sloc = str(unit.root.sloc_range)
    gps_assert(sloc, "1:1-5:7")
    #  Add new line
    loc = ed.at(3, 0)
    ed.insert(loc, "   B : Boolean;\n")
    unit2 = ed.get_analysis_unit()
    sloc2 = str(unit2.root.sloc_range)
    gps_assert(sloc2, "1:1-6:7")
    # Check if prev unit is update
    sloc = str(unit.root.sloc_range)
    gps_assert(sloc, "1:1-6:7")
    ed.undo()
    prj_view_changed = hook('project_view_changed')
    GPS.Project.load("empty/empty.gpr", force=True)
    yield prj_view_changed
    gps_assert(GPS.Project.root().name(), "empty")
    # Check if access to cached unit doesn't raise any exception
    sloc = str(unit.root.sloc_range)
    gps_assert(sloc, "1:1-6:7")
