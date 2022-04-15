"""
This test verifies that searching in the in all the projects
also searches in .gpr files.
"""

from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs

EXPECTED = "a.gpr:1:7"

@run_test_driver
def driver():
    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.FILES_FROM_PROJECT)
    s.pattern.set_text("imported")
    yield wait_idle()
    yield s.yield_find_all()
    yield wait_idle()

    messages = GPS.Locations.list_locations("Search for: imported", "a.gpr")
    gps_assert(str(messages[0]), EXPECTED, "Wrong search result")