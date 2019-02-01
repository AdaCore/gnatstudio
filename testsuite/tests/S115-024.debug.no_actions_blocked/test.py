"""
This test checks verifies that teh GPS task manager is still able
to execute actions when the debugger is busy.
"""

import GPS
from gps_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.Preference("GPS6-Debugger-Debugger-Kind").set("Gdb_MI")

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    # Run the debugger
    dialog = Debug_Run_Dialog()
    yield dialog.open_and_yield()
    yield dialog.ok()

    # Execute the 'debug continue' action asynchronously
    GPS.execute_asynchronous_action("debug continue")

    # Verifiy that we are still able to perform actions
    buf = GPS.EditorBuffer.get(GPS.File('main.adb'))
    GPS.MDI.get_by_child(buf.current_view()).raise_window()
    GPS.execute_action("goto end of buffer")
    gps_assert(buf.current_view().cursor(), buf.end_of_buffer(),
               "'Goto end of buffer' action has not been executed")
