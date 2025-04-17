"""
Test for the actions that open separate editors for log files.
"""
import GPS
from gs_utils.internal.utils import *
import os
import os.path


@run_test_driver
def run_test():
    yield wait_tasks()

    # Open the ALS log file: check that an editor has been opened for it
    GPS.execute_action("Open ALS log file")
    gps_assert(
        GPS.EditorBuffer.get().file().base_name(),
        "inout.txt",
        "The ALS log file was not opened",
    )

    # Open the clangd log file: check that an editor has been opened for it
    GPS.execute_action("Open clangd log file")
    gps_assert(
        GPS.EditorBuffer.get().file().base_name().startswith("clangd."),
        True,
        "The clangd log file was not opened",
    )

    # We are in testsuite mode, we don't have any GS log file: check that
    # the filter for the corresponding action works fine
    gps_assert(
        GPS.Action("Open GNAT Studio log file").can_execute(),
        False,
        "The action to open a GS log file should not be available in testsuite mode",
    )
