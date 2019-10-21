"""
This test checks that 'find next' does not clear the Locations view
when this one has search results messages.
"""

import GPS
from gps_utils.internal.utils import *
import gps_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    s = dialogs.Search()
    yield s.open_and_yield()

    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.pattern.set_text('Hello')

    GPS.execute_action("find all")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(len(GPS.Locations.list_categories()) != 0,
               True,
               "Locations view should have search results after find all")

    GPS.execute_action("find next")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(len(GPS.Locations.list_categories()) != 0,
               True,
               "Locations view should still have search results after " +
               "find next")
