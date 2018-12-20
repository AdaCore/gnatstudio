"""
This test verifies that the selection/location displayed in the status bar
is correct when navigating in the file via the arrow keys.
"""

from GPS import *
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    button = get_widget_by_name("Status Bar Cursor Location")
    gps_assert(button is not None,
               True,
               "Can't retrieve the button: check the Set_Name")
    gps_assert(button.get_label(),
               "1:1",
               "Wrong label at startup")

    # Movement without selection
    buf.current_view().goto(buf.at(1, 2))
    yield wait_tasks(other_than=known_tasks)
    gps_assert(button.get_label(),
               "1:2",
               "Wrong label after moving the cursor")

    send_key_event(Gdk.KEY_Right)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(button.get_label(),
               "1:3",
               "Wrong label after Key right")

    send_key_event(Gdk.KEY_Down)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(button.get_label(),
               "2:3",
               "Wrong label after Key down")

    # Movement with selection
    buf.select(buf.at(1, 1), buf.at(1, 2))
    yield wait_tasks(other_than=known_tasks)
    gps_assert(button.get_label(),
               "(1 line, 1 char) 1:2",
               "Wrong label after selecting 1 character")

    buf.select(buf.at(1, 1), buf.at(1, 5))
    yield wait_tasks(other_than=known_tasks)
    gps_assert(button.get_label(),
               "(1 line, 4 chars) 1:5",
               "Wrong label after selecting multiple characters")

    buf.current_view().goto(buf.at(1, 2))
    yield wait_tasks(other_than=known_tasks)
    gps_assert(button.get_label(),
               "1:2",
               "Wrong label after moving the cursor")

    send_key_event(Gdk.KEY_Down, shift=True)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(button.get_label(),
               "(2 lines, 17 chars) 2:2",
               "Wrong label after selecting Key down + Shift")
