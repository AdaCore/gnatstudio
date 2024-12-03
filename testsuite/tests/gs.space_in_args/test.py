"""Test `-X=-O2 -Werror` in compilation flags """
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    GPS.BuildTarget("Build All").execute(force=True)
    yield wait_tasks()

    gps_assert(
        "process terminated successfully" in GPS.Console("Messages").get_text(), True
    )
