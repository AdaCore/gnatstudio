from gps_utils.internal.utils import run_test_driver, gps_assert, \
    wait_tasks, wait_for_mdi_child
from pygps import get_widgets_by_type
from workflows.promises import timeout


@run_test_driver
def driver():
    GPS.Preference("Debugger-Execution-Window").set(False)

    # This test launches a debug session, and runs the program in the
    # debugger. The goal is to check against a memory corruption that
    # occurs after the process has terminated - the test itself might
    # not report an issue, but valgrind might.
    GPS.execute_action("Build & Debug Number 1")
    yield wait_tasks()
    GPS.Debugger.get().non_blocking_send("run")
    yield timeout(2000)

    yield wait_for_mdi_child("Debugger Execution")

    # sanity check that the test ran
    gps_assert("Put_Line\n" * 4 in GPS.Debugger.get().get_console().get_text(),
               True,
               "debug output not visible in console")
