"""
This test checks casing of replaced string which contains keywords.
"""

import GPS
from gps_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.Preference("Ask-Confirmation-For-Replace-All").set(False)
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.regexp.set_active(True)
    s.pattern.set_text('new Generic_Heap_Vector[ ]*\(([^,]+),[^)]*\)')
    s.replace_text.set_text('new Generic_Heap_Vector (\\1)')

    yield s.yield_replace_all()
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.get_chars(buf.at(5, 4), buf.at(5, 86)),
               "package P_MC2_CMS_Sub_Sub_Step_V is new " +
               "Generic_Heap_Vector (Mc2_Cms_Sub_Sub_Step);",
               "Replace does not work")
    buf.undo()
