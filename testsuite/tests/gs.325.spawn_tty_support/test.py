"""This test verifies that the "run" console is seen as a tty
   from the point of view of programs being launched.
"""
from gs_utils.internal.utils import (
    run_test_driver,
    wait_until_true,
    dot_exe,
    timeout,
    gps_assert,
)
import sys


@run_test_driver
def driver():
    # Build and debug
    GPS.execute_action("Build & Run Number 1")
    # Wait until the run window has appeared
    yield wait_until_true(lambda: GPS.MDI.get("Run: t" + dot_exe) != None)

    # Wait until the text contains "stdout"
    while True:
        text = GPS.Console("Run: t" + dot_exe).get_text()
        if "stdout" in text:
            # Write text to "console.txt"
            with open("console.txt", "w") as f:
                f.write(text)
            break
        yield timeout(100)
