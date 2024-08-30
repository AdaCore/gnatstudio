"""
Test that `locations remove selection` deletes only selected locations
"""

from GPS import *
from gs_utils.internal.utils import *

expected_all = [
    "References for C (a.ads:5) (3 items in 3 files)",
    [
        "a.ads (1 item)",
        ["<b>5:14</b>      [reference] procedure <b>C</b>;"],
        "a.adb (1 item)",
        ["<b>5:14</b>      [reference] procedure <b>C</b> is"],
        "foo.adb (1 item)",
        ["<b>20:6</b>      [call] A.<b>C</b>;"],
    ],
]

expected_del = [
    "References for C (a.ads:5) (2 items in 2 files)",
    [
        "a.adb (1 item)",
        ["<b>5:14</b>      [reference] procedure <b>C</b> is"],
        "foo.adb (1 item)",
        ["<b>20:6</b>      [call] A.<b>C</b>;"],
    ],
]


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("a.ads"))
    buf.current_view().goto(buf.at(5, 14))
    GPS.execute_action("find all references")
    yield hook("language_server_response_processed")
    yield wait_tasks(other_than=known_tasks)

    gps_assert(
        dump_locations_tree(),
        expected_all,
        "'find all references' does not display all locations",
    )

    tree = pygps.get_widgets_by_type(Gtk.TreeView, GPS.MDI.get("Locations").pywidget())[
        0
    ]
    selection = tree.get_selection()
    selection.unselect_all()
    selection.select_path("0:0")
    selection.select_path("0:0:0")
    yield wait_idle()

    GPS.execute_action("locations remove selection")
    gps_assert(
        dump_locations_tree(),
        expected_del,
        "'locations remove selection' wrong deletion",
    )
