"""Check that refactoring/name parameters works properly"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    yield wait_tasks()
    # Open bla.adb, goto declaration of "Create"
    b = GPS.EditorBuffer.get(GPS.File("bla.adb"))
    b.current_view().goto(b.at(6, 19))
    yield timeout(2000)
    select_editor_contextual("Refactoring/Name parameters")
    yield hook("language_client_response_sent")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        b.get_chars(b.at(6, 4), b.at(6, 34)),
        "Ada.Text_IO.Create (File => F);",
        "Does not work",
    )

    b.undo()
