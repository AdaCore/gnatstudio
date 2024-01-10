"""
This test checks that we can attach to a running process, detach from
it with the process still running, and re-attach to it after.
"""
import GPS
import os
from gs_utils.internal.utils import *

ATTACH_DIALOG_NAME = "Enter the PID of the process to attach to"


@run_test_driver
def test_driver():
    # Build the project
    GPS.execute_action("Build All")
    yield wait_tasks()

    # Run the process
    proc = GPS.Process("./obj/main_t819_019")
    pid = proc.get_pid()

    # Initialize a debug sessions without any main
    GPS.execute_action("/Debug/Initialize/no main file")
    yield wait_idle()

    # Attach to the running 'main' process
    yield idle_modal_dialog(lambda: GPS.execute_action("debug attach"))
    dialog = get_window_by_title(ATTACH_DIALOG_NAME)
    entry = get_widgets_by_type(Gtk.Entry, dialog)[0]
    ok_button = get_button_from_label("OK", dialog)
    entry.set_text(str(pid))
    ok_button.clicked()
    yield wait_DAP_server("stackTrace")
    yield wait_idle()

    # Check that the debugger has been stopped when attaching to
    # the running process
    debugger = GPS.Debugger.get()
    frames = debugger.frames()
    gps_assert(
        len(frames) != 1,
        True,
        "The debugger should be stopped, and thus we should have frames",
    )

    # Check that the  action that terminate the debugger and the debuggee
    # are not available when a debuggee is attached: the 'debug detach' action
    # and toolbar buttons should be used instead.
    gps_assert(
        GPS.Action("terminate debugger").can_execute(),
        False,
        "The 'terminate debugger' action should not be available when a "
        + "debuggee is attached",
    )
    gps_assert(
        GPS.Action("terminate all debuggers").can_execute(),
        False,
        "The 'terminate all debuggers' action should not be available when a "
        + "debuggee is attached",
    )

    # Detach the debugger from the debuggee: check we have exited the 'Debug'
    # perspective and that the process is still running
    GPS.execute_action("debug detach")
    yield wait_until_true(lambda: GPS.MDI.current_perspective() != "Debug")
    gps_assert(
        GPS.MDI.current_perspective(),
        "Default",
        "We should have exited the 'Debug' perspective after detaching",
    )
    gps_assert(
        len(proc.list()),
        1,
        "The process should still be running after detaching the debugger",
    )

    # Kill the process manually
    proc.kill()
