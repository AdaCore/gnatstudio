"""
This test checks that the 'replace all' action works well
when the search and replace strings differ in lengths, even
when the buffers that contain the occurences haven't been
opened yet.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


EXPECTED_AFTER_REPLACE = "Bom Dia Bom Dia Bom Dia"


@run_test_driver
def test_driver():
    s = dialogs.Search()
    yield s.open_and_yield()

    s.set_scope(dialogs.Search.Context.FILES_FROM_PROJECT)
    s.pattern.set_text("Hello")
    s.replace_text.set_text("Bom Dia")

    GPS.execute_action("replace all")
    yield wait_tasks(other_than=known_tasks)

    buffer = GPS.EditorBuffer.get(GPS.File("main.adb"))

    gps_assert(
        buffer.get_chars().rstrip(),
        EXPECTED_AFTER_REPLACE,
        "The 'replace all' did not work properly",
    )
