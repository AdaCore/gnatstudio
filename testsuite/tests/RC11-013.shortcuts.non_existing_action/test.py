"""
This test verifies that only a message is displayed when a short-cut is linked
to a non-existing action.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    console = GPS.Console("Messages")
    console.clear()
    # Build Main Number 1 is not defined if test.gpr doesn't defined a main
    send_key_event(Gdk.KEY_F4)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(console.get_text(),
               "Action not defined : Build Main Number 1\n",
               "Wrong warning displayed in the Messages view")
