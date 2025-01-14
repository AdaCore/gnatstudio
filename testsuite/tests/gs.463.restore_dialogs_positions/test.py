"""
Test that we adjust dialog's position when it has
an off-screen position from the history.
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def run_test():
    b = GPS.EditorBuffer.get(GPS.File("bar.adb"))
    yield wait_idle()

    # Open the search view
    d = dialogs.Search()
    yield d.open_and_yield()
    yield wait_idle()

    yield d.yield_close()
    yield wait_idle()
