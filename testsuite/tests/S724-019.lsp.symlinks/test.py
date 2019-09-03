import os
from gps_utils.internal.utils import run_test_driver, wait_tasks, known_tasks


@run_test_driver
def driver():
    GPS.Preference("Prj-Editor-Trusted-Mode").set(False)
    # Open the legit source file
    GPS.EditorBuffer.get(GPS.File("src/main.adb"))

    # Open the symlinked file
    GPS.EditorBuffer.get(
        GPS.File(os.path.join(GPS.pwd(), "not_src", "main.adb")))

    # There should be no errors.
    yield wait_tasks(other_than=known_tasks)
