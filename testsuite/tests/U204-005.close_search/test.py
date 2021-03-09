"""
Test the focus passing when closing the search dialog 
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def run_test():
    GPS.EditorBuffer.get(File("bar.adb"))
    GPS.EditorBuffer.get(File("bar.ads"))
    yield wait_tasks()

    execute_action("/Window/Floating")
    b = GPS.EditorBuffer.get(File("foo.adb"))
    yield wait_idle()

    s = dialogs.Search()
    yield s.open_and_yield()
    yield timeout(100)
    execute_action("exit search")

    current_view = GPS.MDI.current()
    gps_assert(current_view.name(short=True), "foo.adb",
               "foo.adb should be focused")
