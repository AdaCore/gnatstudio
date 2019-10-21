"""
This test checks the good behavior of the 'Replace all'
action.
"""

import GPS
from gps_utils.internal.utils import *
import gps_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    GPS.Preference("Ask-Confirmation-For-Replace-All").set(False)
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.CURRENT_FILE)
    s.pattern.set_text('Hello')
    s.replace_text.set_text('Hi')

    GPS.execute_action("replace all")
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.get_chars(), "Hi Hi Hi\n",
               "'Replace all' action failed")
