"""
Test the actions "next/previous tag (same weight)"
"""

from GPS import *
from gs_utils.internal.utils import *


def verify_action(selection, start, backward, expected):
    selection.unselect_all()
    selection.select_path(start)

    for step in expected:
        if backward:
            GPS.execute_action("previous tag (same weight)")
        else:
            GPS.execute_action("next tag (same weight)")
        model, selected = selection.get_selected_rows()
        path = selected[0]
        gps_assert(str(path),
                   step,
                   "Issue detected, backward: " + str(backward))


@run_test_driver
def run_test():
    file_a = GPS.File("a.adb")
    file_b = GPS.File("b.adb")
    category = "Hello_World"

    # Create multiple messages of importance 4 and 5
    for file in [file_a, file_b]:
        GPS.Message(category, file, 1, 1, "1", importance=5)
        GPS.Message(category, file, 2, 1, "2", importance=4)
        GPS.Message(category, file, 3, 1, "3", importance=5)

    # Create only one message of importance 3
    GPS.Message(category, file_b, 4, 1, "4", importance=3)

    # Retrieve the Locations view selection
    locations = GPS.MDI.get("Locations")
    tree = get_widgets_by_type(Gtk.TreeView, locations.pywidget())[0]
    selection = tree.get_selection()

    # Move forward between files
    verify_action(selection,
                  "0",
                  False,
                  ["0:0:0", "0:0:2", "0:1:0", "0:1:2", "0:0:0"])

    # Move backward between files
    verify_action(selection,
                  "0:0:1",
                  True,
                  ["0:1:1", "0:0:1"])

    # Alone => should not move
    verify_action(selection,
                  "0:1:3",
                  False,
                  ["0:1:3", "0:1:3"])
