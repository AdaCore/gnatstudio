"""Verify the correct function of the restart of the language server
   after modifying the environment through GPS.setenv.
"""
from gs_utils.internal.utils import run_test_driver, wait_tasks, hook, \
    gps_assert, timeout
from workflows.promises import known_tasks

import os


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.current_view().goto(b.at(4, 7))
    yield wait_tasks(other_than=known_tasks)

    # First verify that the navigation does *not* work
    GPS.execute_action('goto declaration')
    # At this point "language_server_response_processed" shouldn't work,
    # timeout instead
    yield timeout(500)
    current_buf = GPS.EditorBuffer.get()
    gps_assert(current_buf.file(), GPS.File('main.adb'),
               "'goto declaration' should not have worked at this point")

    # Now set the project path and reload the project
    GPS.setenv("GPR_PROJECT_PATH", os.path.join(GPS.pwd(), "subdir"))
    GPS.Project.load("p.gpr")

    # Restart the language server
    GPS.LanguageServer.get_by_language_name("Ada").restart()
    yield wait_tasks(other_than=known_tasks)

    # Verify that the navigation works now
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.current_view().goto(b.at(4, 7))
    GPS.execute_action('goto declaration')
    yield timeout(500)
    gps_assert(GPS.EditorBuffer.get().file(), GPS.File('foo.ads'),
               "'goto declaration' did not open the right file")
