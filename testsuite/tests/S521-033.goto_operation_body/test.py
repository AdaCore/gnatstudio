"""
This test verifies that goto body action works with overriding operators.
"""

from GPS import *
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    # Build the executable first
    GPS.execute_action("Build Main Number 1")
    yield wait_tasks()

    # Open the main source file
    b = GPS.EditorBuffer.get(GPS.File("a.adb"))

    # Set cursor on "and" operator declaration
    v = b.current_view()
    v.goto(b.at(10, 14))

    # goto body
    GPS.execute_action("goto body")
    yield wait_idle()

    # Verify that cursor is set to the operator body
    gps_assert(
        v.cursor().line(), 14,
        "Goto body of overridding operator works incorrect.")
