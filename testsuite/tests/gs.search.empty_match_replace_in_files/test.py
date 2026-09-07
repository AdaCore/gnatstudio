"""
Replacing with a regexp that matches the empty string in a file that is
not open in an editor must not raise: the matched slice of an empty match
is empty, and its end must not be read.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    GPS.Preference("Ask-Confirmation-For-Replace-All").set(False)

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.FILES_FROM_PROJECT)
    s.regexp.set_active(True)
    s.pattern.set_text("X*")
    s.replace_text.set_text("")

    GPS.execute_action("replace all")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        open(GPS.File("main.adb").path).read(),
        "procedure Main is\nbegin\n   null;\nend Main;\n",
        "replacing an empty match changed the file",
    )
