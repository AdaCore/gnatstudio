"""Test the pressing of Enter in a Run console after interrupting the run"""

from gps_utils.internal.utils import run_test_driver, timeout, \
    send_key_event, GDK_RETURN
import pygps
import sys


@run_test_driver
def driver():
    # Execute build & run
    GPS.execute_action("Build & Run Number 1")

    window = None

    # Wait until the main is running in the output window
    while not window:
        yield timeout(100)
        window = GPS.MDI.get("Run: hello"
                             + ".exe" if sys.platform == "win32" else "")

    view = pygps.get_widgets_by_type(Gtk.TextView, window.pywidget())[0]

    # Interrupt the task
    [t for t in GPS.Task.list() if t.name() == "Run Main"][0].interrupt()

    # Send a couple of Enter keys
    view.grab_focus()
    yield timeout(100)
    send_key_event(GDK_RETURN)
    yield timeout(100)
    send_key_event(GDK_RETURN)
