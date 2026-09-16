"""
For eng/ide/gnatstudio#687.

While a debugger is stopped, the line under the cursor is given a
"Continue to line" message, registered in a side column belonging to the
debugger. Terminating the debugger removes that column while the message
is still registered in it, and that must not trip the tampering check of
the container holding the column's messages.
"""

import GPS
from gs_utils.internal.utils import *

CATEGORY = "debugger-run-to-line"


@run_test_driver
def test_driver():
    yield wait_tasks()

    # Open the editor before starting the debugger, so that it is given
    # the debugger's "Continue to line" column

    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(6, 1))
    yield wait_idle()

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    debug = GPS.Debugger.get()
    for cmd in ["b main.adb:6", "run"]:
        yield wait_until_not_busy(debug)
        debug.send(cmd)
    yield wait_until_not_busy(debug)

    # Moving the cursor inside the frame being debugged is what registers
    # the message in that column

    buf.current_view().goto(buf.at(8, 1))
    yield wait_idle()
    gps_assert(
        len(GPS.Message.list(category=CATEGORY)),
        1,
        "A 'Continue to line' message should have been registered",
    )

    debug.close()
    yield wait_idle()
    gps_assert(
        GPS.Message.list(category=CATEGORY),
        [],
        "The 'Continue to line' message should be gone with the debugger",
    )
