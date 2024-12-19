"""
This test checks that we correctly invoke gnatcheck on non-native
projects.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("gnatcheck root project")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        "-rules " in GPS.Console().get_text().splitlines()[0],
        False,
        "The -rules option should not be speicified again, since "
        + "it's already specified in the .gpr file",
    )
