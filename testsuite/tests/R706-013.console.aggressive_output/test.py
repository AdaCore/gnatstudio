"""Test that GPS is responsive enough to interrupt a process that's outputting
   information very aggressively
"""

from gps_utils.internal.utils import run_test_driver, timeout, wait_idle, \
    send_key_event, GDK_RETURN
import pygps
import sys


@run_test_driver
def driver():
    # Execute build & run
    GPS.execute_action("Build & Run Number 1")

    window = None

    # Wait until the main is running in the output window
    while not window:
        yield timeout(100)
        window = GPS.MDI.get("Run: hello"
                             + ".exe" if sys.platform == "win32" else "")

    # Wait one second...
    yield timeout(1000)

    # Then wait until idle.
    # What we're trying to do here is verify that the main loop has cycles
    # to respond to the requests, ie it should become idle before we hit
    # a timeout, even if the process being launched is outputing characters
    # for 3 minutes

    # TODO: find a way to check that the main loop is responsive

    # Interrupt the task
    [t for t in GPS.Task.list() if t.name() == "Run Main"][0].interrupt()
