"""
Test the lal_view plugin
"""
from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.execute_action("open Libadalang")

    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    loc = buf.at(1, 12)
    # Click in the buffer to see the tree
    buf.current_view().goto(loc)
    yield wait_idle()

    # Activate Full mode
    get_widget_by_name("lal_view full toggle").clicked()
    yield wait_idle()

    t = get_widget_by_name("lal_view tree")
    gps_assert(buf.current_view().cursor(), loc, "The cursor should not have moved")
    click_in_tree(t, path="0:1:1:4", events=pygps.double_click_events)
    gps_assert(
        buf.current_view().cursor(),
        buf.at(4, 5),
        "Double click should have moved the cursor",
    )
