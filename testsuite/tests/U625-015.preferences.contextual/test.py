"""
Opening the "local preference" and destroying the menu should not crash.
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs
from gs_utils.internal.editor import click_in_widget


@run_test_driver
def run_test():
    GPS.execute_action("open Locations")
    loc = GPS.MDI.get("Locations")

    GPS.EditorBuffer.get(GPS.File("foo.adb"))
    editor = GPS.MDI.get("foo.adb")
    editor.raise_window()
    editor.pywidget().grab_focus()

    # Open the local preferences menu
    m = get_widget_by_name("local-config", loc.pywidget())
    click_in_widget(m.get_child().get_event_window(), button=1)
    # Close the menu
    send_key_event(GDK_ESCAPE)

    # Destroy the focused widget when the menu was spawned
    editor.close(force=True)
    # Destroy the view related to the menu
    loc.close(force=True)
