"""
This test verifies that spawning a debugger from the Python console
does not make the Python console the only widget that can grab events
(e.g: all the other MDI children should be responsive too).
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Build the executable first

    GPS.execute_action("Build Main Number 1")
    yield wait_tasks()

    # Open the Python console
    GPS.execute_action("open Python")
    console = GPS.Console("Python")

    # Copy-paste the Python command used to spawn the debugger and copy
    # it in the Python console.
    # Writing directly in the command does not trigger the different event
    # handlers of the Python console, that's why we need to copy-paste the
    # command.

    GPS.Clipboard.copy('GPS.execute_action("debug initialize Default:a")')

    GPS.execute_action("Paste from Clipboard")

    # Press enter to execute the Python command

    send_key_event(GDK_RETURN)

    # Verify that no widget is grabbing user events

    gps_assert(
        Gtk.grab_get_current(), None,
        "The Python console is grabbing all the user events after " +
        "spawning the debugger.")
