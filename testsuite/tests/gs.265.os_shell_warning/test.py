"""
A warning should be shown when OS Shell can't be started because of the env.
"""

import os
import os.path
import os_utils
from gs_utils.internal.utils import run_test_driver, gps_assert, wait_tasks


@run_test_driver
def driver():
    if os.name == 'nt':
        os.environ["COMSPEC"] = ""
    else:
        os.environ["TERM"] = ""

    GPS.execute_action("open os shell")
    yield wait_tasks()

    gps_assert("Can't start OS shell" in GPS.Console("Messages").get_text(),
               True,
               "Missing error in the Messages view")
