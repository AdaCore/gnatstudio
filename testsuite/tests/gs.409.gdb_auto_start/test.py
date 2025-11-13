"""
Tests that we have Run dialog when Debugger-Auto-Start
preference is Dialog_Run and debuggee is started when
Debugger-Auto-Start = Run.
"""

import GPS
from gs_utils.internal.utils import *

expect = "Debugger is initialized: you can now start the debuggee by clicking on the 'Debug Run' toolbar button."


@run_test_driver
def driver():
    GPS.Preference("Debugger-Launch-Setup-Command").set("Run_with_dialog")
    yield wait_tasks()
    dialog = Debug_Run_Dialog()
    yield dialog.open_with_action("/Debug/Initialize/main")

    dialog = None
    while dialog is None:
        yield timeout(50)
        dialog = get_window_by_title("Run/Start")

    get_button_from_label("Cancel", dialog).clicked()
    yield timeout(100)

    debug = GPS.Debugger.get()
    debug.close()
    yield wait_tasks()

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.current_view().goto(b.at(6, 1))
    GPS.execute_action("debug set line breakpoint")
    GPS.Preference("Debugger-Launch-Setup-Command").set("Run")
    yield wait_idle()

    yield GPS.execute_action("/Debug/Initialize/main")
    yield wait_until_not_busy(debug)
    gps_assert(b.current_view().cursor(), b.at(6, 1))

    debug = GPS.Debugger.get()
    debug.close()
    yield wait_tasks()

    GPS.Preference("Debugger-Launch-Setup-Command").set("None")
    yield wait_idle()
    yield GPS.execute_action("/Debug/Initialize/main")
    yield wait_for_mdi_child("Debugger Console")
    yield wait_idle()

    console = GPS.Debugger.get().get_console()
    text = console.get_text()
    gps_assert(expect in text, True, "No message:\n" + text)
