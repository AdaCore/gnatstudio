"""
Send a break command with an invalid value: no error should be reported in 
the traces.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    main = GPS.File("main.adb")
    editor = GPS.EditorBuffer.get(main)

    editor.current_view().goto(editor.at(5, 1))

    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks(other_than=known_tasks)

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    debug = GPS.Debugger.get()
    debug.send("b zzyyxxkk")
