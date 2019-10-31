"""
Set an inexistent target in IDE'Program_Host and verify that an error is
properly raised to the user (This case only arises when an user sets a target
and is not using a gdb_server: normally the remote connect is timeout).
Also verify that the load command is not triggered after failing to connect.
"""
import GPS
from gs_utils.internal.utils import *

error = "foobar: No such file or directory."
load = "load"


@run_test_driver
def test_driver():
    GPS.Preference("Debugger-Load-On-Init").set(True)
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    debug = GPS.Debugger.get()
    output = debug.get_console().get_text()
    gps_assert(error in output,
               True,
               "Missing error in the output")
    gps_assert(load in output,
               False,
               "Don't try to load after failing to connect")
