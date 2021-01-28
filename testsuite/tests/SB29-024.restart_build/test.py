#  Checking whether "Restart build all" action works

from GPS import *
from gs_utils.internal.utils import *

flag = False
counter = 0


@gs_utils.hook("compilation_starting")
def __starting(*args):
    global flag
    flag = not flag
    return True


@run_test_driver
def driver():
    global flag
    global counter
    GPS.EditorBuffer.get(GPS.File("main.adb"))

    # Start the regular build
    GPS.execute_action("Build All")

    # Waiting until building is started
    yield wait_until_true(lambda: flag)

    # Interrupting the first one and start another
    GPS.execute_action("Restart build all")

    # Waiting until building is started once more
    yield wait_until_true(lambda: not flag)

    # Check whether we don't have two building process at the same time
    for task in GPS.Task.list():
        if 'Build' in task.name():
            counter += 1
    gps_assert(counter < 2, True, "Building is not interrupted")
