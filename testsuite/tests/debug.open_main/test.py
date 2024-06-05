"""
#112
This test checks that the main file is opened when
debugging is started
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    gps_assert(
        GPS.EditorBuffer.get(GPS.File("main.adb"), open=False) is not None,
        True,
        "main.adb should be opened",
    )
