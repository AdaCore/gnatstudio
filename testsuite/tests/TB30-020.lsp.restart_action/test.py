"""
This test checks the good behvior of the actions used to restart the
language servers.
"""
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    gps_assert(GPS.Action("restart ada language server").exists(), True,
               "The action to restart the ALS is not present")
    gps_assert(GPS.Action("restart c language server").exists(), True,
               "The action to restart clangd is not present")

    # Restart the ALS and wait for the language_server_started hook.
    # The test will timeout if the server has not been restarted.
    GPS.execute_action("restart ada language server")
    yield hook('language_server_started')

    # Load a pure Ada project: verify that the action to restart clangd
    # has been removed

    GPS.Project.load("other.gpr")
    yield hook('language_server_stopped')
    yield wait_idle()

    gps_assert(GPS.Action("restart c language server").exists(), False,
               "The action to restart clangd should not be present")
