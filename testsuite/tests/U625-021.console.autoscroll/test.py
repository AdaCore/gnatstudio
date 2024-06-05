"""
Test the autoscroll behavior of a Console.
"""

from gs_utils.internal.utils import *
from GPS import *


@run_test_driver
def run():
    console = GPS.Console("Messages")
    console.clear()
    yield wait_tasks(other_than=known_tasks)
    view_widget = console.pywidget()
    win = get_widgets_by_type(Gtk.ScrolledWindow, view_widget)[0]
    gps_assert(
        win.get_vadjustment().get_value(),
        0,
        "Wrong vertical adjustment for empty Console",
    )

    # By default autoscroll is enabled. Thus inserting some lines should
    # move the vertical adjustment.
    for i in range(1, 100):  # Insert at least 100 lines to force the scroll
        console.write(str(i) + "\n")
    yield wait_idle()

    expected = win.get_vadjustment().get_value()
    gps_assert(expected > 0, True, "Wrong vertical adjustment for autoscrolling")

    # Disable the autoscroll behavior and insert some lines = the vertical
    # adjustment should be the same
    console.set_automatic_scroll(False)

    for i in range(1, 100):
        console.write(str(i) + "\n")
    yield wait_idle()

    gps_assert(
        win.get_vadjustment().get_value(),
        expected,
        "Wrong vertical adjustment without autoscrolling",
    )
