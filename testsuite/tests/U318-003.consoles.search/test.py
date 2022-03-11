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
import re

SEARCH_TAG_REGEXP = r'<apply_tag name="Search results">(.*)<\/apply_tag>'


def is_word_highlighted(buffer, word, line):
    start = buffer.get_iter_at_line(line)
    end = start.copy()
    end.forward_to_line_end()
    format = buffer.register_serialize_tagset()
    serialized_text = str(buffer.serialize(buffer, format, start, end))
    return re.search(SEARCH_TAG_REGEXP, serialized_text) != None


@run_test_driver
def run_test():
    # Build and Run the main to get the 'run' console
    GPS.execute_action("Build & Run Number 1")
    yield wait_until_true(lambda: MDI.get("Run: main" + dot_exe) != None)
    console = MDI.get("Run: main" + dot_exe).get_child().pywidget()
    buffer = get_widgets_by_type(Gtk.TextView, console)[0].get_buffer()
    search_entry = get_widgets_by_type(Gtk.SearchEntry, console)[0]

    # Search for 'nec': verify that it's correctly selected
    search_entry.set_text("nec")
    search_entry.grab_focus()
    search_entry.select_region(-1, -1)
    yield timeout(300)
    gps_assert(
        is_word_highlighted(buffer, "nec", 1),
        True,
        "'nec' should be highlighted at line 1",
    )

    # Press ENTER to go to the next occurrence
    send_key_event(GDK_RETURN)
    yield timeout(300)

    gps_assert(
        is_word_highlighted(buffer, "nec", 5),
        True,
        "'nec' should be highlighted at line 5",
    )

    # Press backspace and verify that we are still on the same
    # occurrence, but without the 'c' now
    send_key_event(GDK_BACKSPACE)
    yield timeout(300)

    gps_assert(
        is_word_highlighted(buffer, "ne", 5),
        True,
        "'nec' should be highlighted at line 5",
    )

    # Press ENTER again: we should have not any selected occurence anymore
    send_key_event(GDK_RETURN)
    yield timeout(300)

    gps_assert(
        search_entry.get_style_context().has_class("error"),
        True,
        "The search entry should be highlighted in red ('error' CSS class)",
    )

    # Change the text: verify that we start the search from the start of the
    # buffer again
    search_entry.set_text("port")
    yield timeout(300)
    gps_assert(
        is_word_highlighted(buffer, "port", 2),
        True,
        "'port' should be highlighted at line 2",
    )

    # Add a character: we should match the same occurence, but with one more
    # char
    search_entry.select_region(-1, -1)
    send_key_event(ord("a"))
    yield timeout(300)
    gps_assert(
        is_word_highlighted(buffer, "porta", 2),
        True,
        "'porta' should be highlighted at line 2",
    )
