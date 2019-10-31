"""
Verify that GPS is not confused by "fake (gdb) prompts" => no freeze.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    debug = GPS.Debugger.get()
    debug.send("start", show_in_console=True)
    yield wait_until_not_busy(debug)
    debug.send("show prompt", show_in_console=True)
    yield wait_until_not_busy(debug)
    debug.send("print \n(gdb)", show_in_console=True)
    yield wait_until_not_busy(debug)
