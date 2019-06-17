"""
Double-clicking on a file row in the Locations view should open the
corresponding file.
"""

from GPS import *
from gps_utils.internal.utils import *

BODY = "pack.adb"
NAME_COLUMN = 5


@run_test_driver
def run_test():
    # Build the executable first, needed for "find all references" with the
    # old engine
    GPS.execute_action("Build Main Number 1")
    yield wait_tasks()

    buf = GPS.EditorBuffer.get(GPS.File("a.adb"))
    buf.current_view().goto(buf.at(2, 7))
    GPS.execute_action("find all references")
    yield wait_tasks(other_than=known_tasks)

    # If "find all references" has opened BODY, close it
    if GPS.MDI.get(BODY):
        GPS.MDI.get(BODY).close(force=True)

    locations = GPS.MDI.get("Locations")
    tree = get_widgets_by_type(Gtk.TreeView, locations.pywidget())[0]

    path = find_in_tree(tree, NAME_COLUMN, BODY)
    click_in_tree(tree, path=path, events=pygps.single_click_events)
    gps_assert(GPS.MDI.get(BODY),
               None,
               "Simple clicking on a file row should not open the file")
    click_in_tree(tree, path=path, events=pygps.double_click_events)
    gps_assert(GPS.MDI.get(BODY) is not None,
               True,
               "Double clicking on a file row should open the file")
    expected = len(GPS.MDI.children())
    click_in_tree(tree, path=path, events=pygps.double_click_events)
    gps_assert(len(GPS.MDI.children()),
               expected,
               "File already opened: double-clicking should have no effect")
