"""
This test verifies that search works correctly for interactive
consoles.

In particular we test:

 . Basic search for the first occurrence
 . Check that ENTER goes to the next occurrence
 . Check that BACKSPACE works (same occurrence
   but with less one letter)
 . Check that we highlight the search in red when no
   occurrence is found
 . Check that we restart from the beginning when pressing
   ENTER right after
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def run_test():
    # Build and Run the main to get the 'run' console
    GPS.execute_action("Build & Run Number 1")
    yield wait_tasks()
    console = MDI.get("Run: main").get_child().pywidget()
    buffer = get_widgets_by_type(Gtk.TextView, console)[0].get_buffer()
    search_entry = get_widgets_by_type(Gtk.SearchEntry, console)[0]

    # Search for 'nec': verify that it's correctly selected
    search_entry.set_text("nec")
    yield wait_until_true(lambda: buffer.get_has_selection())
    start, end = buffer.get_selection_bounds()
    gps_assert(buffer.get_text(start, end, False), "nec",
               "The text should be selected")
    gps_assert(start.get_line(), 1, "Wrong line for result")

    # Press ENTER to go to the next occurrence
    search_entry.grab_focus()
    send_key_event(GDK_RETURN)
    yield timeout(500)

    start, end = buffer.get_selection_bounds()
    gps_assert(buffer.get_text(start, end, False), "nec",
               "The text should be selected")
    gps_assert(start.get_line(), 5, "Wrong line for result")

    # Press backspace and verify that we are still on the same
    # occurrence, but without the 'c' now
    send_key_event(GDK_BACKSPACE)
    yield timeout(500)

    start, end = buffer.get_selection_bounds()
    gps_assert(
        buffer.get_text(start, end, False), "ne",
        "Wrong text selected after backspace"
    )
    gps_assert(start.get_line(), 5, "Wrong line for result")

    # Press ENTER again: we should have not any selected occurence anymore
    send_key_event(GDK_RETURN)
    yield timeout(500)

    gps_assert(
        buffer.get_has_selection(),
        False,
        "We should no have any occurrence selected anymore",
    )
    gps_assert(
        search_entry.get_style_context().has_class('error'),
        True,
        "The search entry should be highlighted in red ('error' CSS class)")

    # Change the text: verify that we start the search from the start of the
    # buffer again
    search_entry.set_text("port")
    yield wait_until_true(lambda: buffer.get_has_selection())
    start, end = buffer.get_selection_bounds()
    gps_assert(
        buffer.get_text(start, end, False), "port",
        "The text should be selected")
    gps_assert(start.get_line(), 2, "Wrong line for result after error")

    # Add a character: we should match the same occurence, but with one more
    # char
    search_entry.select_region(-1, -1)
    send_key_event(ord('a'))
    yield timeout(500)
    start, end = buffer.get_selection_bounds()
    gps_assert(
        buffer.get_text(start, end, False), "porta",
        "Wrong selected text after adding char")
    gps_assert(start.get_line(), 2, "Wrong line for result after adding char")
