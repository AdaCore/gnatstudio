"""
This test destroyed a view while its menu is opened.
It only happens for floating view, thus use the Search view.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    # Open the search view
    s = dialogs.Search()
    yield s.open_and_yield()

    # Open the local menu
    m = get_widget_by_name("local-config", s.dialog)
    click_in_widget(m.get_child().get_event_window(), button=1)

    # Destroy the search view
    GPS.execute_action("smart escape")

    # Events should still works: try to write in an editor
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_idle()
    expected = buf.get_chars()
    send_key_event(ord("a"))
    yield wait_idle()
    gps_assert(buf.get_chars(),
               "a" + expected,
               "The event handler is locked")
