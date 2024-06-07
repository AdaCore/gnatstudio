"""
This test verifies that we don't display GDB-mi's results
(i.e: the data sent after "^done") in the Debugger Console
when inserting a breakpoint.
"""
import GPS
from gs_utils.internal.utils import *

EXPECTED = """(gdb) -break-insert -f main.adb:25
(gdb)"""


@run_test_driver
def test_driver():
    # Launch the debugger

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # Open main.adb and set a breakpoint line 25

    main = GPS.File("main.adb")
    editor = GPS.EditorBuffer.get(main)

    GPS.MDI.get("main.adb").raise_window()
    editor.current_view().goto(editor.at(25, 1))

    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks(other_than=known_tasks)

    # Check the console's output
    console = GPS.Debugger.get().get_console()
    text = console.get_text()

    gps_assert(
        EXPECTED in text, True, "The -break-insert output is not the one expected"
    )
