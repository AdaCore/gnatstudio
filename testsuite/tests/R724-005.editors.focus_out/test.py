"""
When a selection is active and the editor focus out then the selection
should be stopped when opening the Search view without crashing the editor.
"""

from GPS import *
from gps_utils.internal.utils import *


TEXT = "Hello"


def get_selection(buf):
    return buf.get_chars(buf.selection_start(), buf.selection_end() - 1)


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_idle()
    view = buf.current_view()
    expected = buf.get_chars()
    default_selection = get_selection(buf)

    buf.select()
    gps_assert(get_selection(buf),
               expected,
               "A selection should have been done")

    # Open the search view to change the focus
    s = dialogs.Search()
    yield s.open_and_yield()
    while get_selection(buf) != default_selection:
        yield timeout(100)

    # The selection should have been stopped and the buffer should still work
    gps_assert(get_selection(buf),
               default_selection,
               "The selection should have been stopped")
    buf.insert(buf.at(1, 1), TEXT)
    buf.delete(buf.at(1, 1), buf.at(1, len(TEXT)))
    yield wait_idle()
    gps_assert(buf.get_chars(),
               expected,
               "The buffer doesn't seems to work properly")
