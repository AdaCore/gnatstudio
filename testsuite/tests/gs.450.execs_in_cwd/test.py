"""
This test checks that GPS.Action.destroy_ui() properly removes the
contextual menus associated with the action.
"""

from gs_utils.internal.utils import (
    run_test_driver,
    wait_tasks,
    known_tasks
)
import glob

@run_test_driver
def run_test():
    # Open a file editor to trigger git actions
    GPS.EditorBuffer.get(GPS.File("main.adb"))

    # Run a build to check that gprbuild is picked up
    GPS.execute_action("Build main number 1")


    yield wait_tasks(other_than=known_tasks)
