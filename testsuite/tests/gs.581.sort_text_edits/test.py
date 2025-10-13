"""Check that refactoring/name parameters works properly"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_1 = "   Foo (I => 1, J => 27, K => 32, Hello => 1000);\n"
EXPECTED_2 = """   Foo (I => 1,
        J => 27,
        K => 32,
        Hello => 1000);
"""


@run_test_driver
def driver():
    yield wait_tasks()
    b = GPS.EditorBuffer.get(GPS.File("bla.adb"))
    b.current_view().goto(b.at(13, 5))
    select_editor_contextual("Refactoring/Name parameters")
    yield hook("language_client_response_sent")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        b.get_chars(b.at(13, 1), b.at(13, 1).end_of_line()),
        EXPECTED_1,
        "Wrong textEdit for flat call",
    )

    b.current_view().goto(b.at(14, 5))
    select_editor_contextual("Refactoring/Name parameters")
    yield hook("language_client_response_sent")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        b.get_chars(b.at(14, 1), b.at(17, 1).end_of_line()),
        EXPECTED_2,
        "Wrong textEdit for multiline call",
    )
