"""
Test the action "locations copy to clipboard".
"""

from GPS import *
from gs_utils.internal.utils import *

expected = [['a.adb:2:6: with Pack;',
             'a.adb:6:4: Pack.Foo;',
             'a.adb:7:4: Pack.Foo;'],
            ['pack.ads:1:9: package Pack is'],
            ['pack.adb:1:14: package body Pack is']]


def build_expected(path_list):
    result = []
    for path in path_list:
        segment = [int(s) for s in path.split(":")]
        if len(segment) == 3:
            result.append(expected[segment[1]][segment[2]])
        elif len(segment) == 2:
            result = result + expected[segment[1]]
    if result == []:
        for file_msg in expected:
            result = result + file_msg
    # Remove the duplicate
    no_duplicate = []
    for msg in result:
        if msg not in no_duplicate:
            no_duplicate.append(msg)
    return "\n".join(no_duplicate)


def get_clipboard_content():
    return GPS.Clipboard.contents()[GPS.Clipboard.current()]


def test_selection(selection, path_list, msg):
    selection.unselect_all()
    for path in path_list:
        selection.select_path(path)
    GPS.execute_action("locations copy to clipboard")
    gps_assert(get_clipboard_content(),
               build_expected(path_list),
               msg)


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("a.adb"))
    buf.current_view().goto(buf.at(2, 7))
    GPS.execute_action("find all references")
    yield hook("language_server_response_processed")
    yield wait_tasks(other_than=known_tasks)

    locations = GPS.MDI.get("Locations")
    tree = get_widgets_by_type(Gtk.TreeView, locations.pywidget())[0]
    selection = tree.get_selection()

    # Executing the action on an empty selection => return all locations
    test_selection(selection, [], "Issue with empty selection")

    # Executing the action on a category node => return the category locations
    test_selection(selection, ["0"], "Issue when selecting a Category")

    # Executing the action on a file node => return the file locations
    test_selection(selection, ["0:0"], "Issue when selecting a File")

    # Executing the action on a message node => return the location
    test_selection(selection, ["0:0:0"], "Issue when selecting a Message")

    # No duplicate when file + message
    test_selection(selection,
                   ["0:0", "0:0:0"],
                   "Issue when overlapping selection")

    # Multi selection of 2 messages
    test_selection(selection,
                   ["0:0:0", "0:0:1"],
                   "Issue with the multiselection")
