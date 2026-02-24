"""
This test navigation after loading a sarif file in the Analysis Report via the
action "Load Sarif File".
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


def test_navigation(
    tree, path, expected_name, expected_line, expected_column, double_click
):
    tree.grab_focus()
    yield wait_idle()
    if double_click:
        click_in_tree(tree, path=path, events=double_click_events)
    else:
        click_in_tree(tree, path=path)
    yield wait_idle()
    current = GPS.MDI.current()
    gps_assert(current.name(short=True), expected_name, "Wrong name for %s" % str(path))
    child = current.get_child()
    if isinstance(child, GPS.EditorView):
        cursor = child.cursor()
        gps_assert(cursor.line(), expected_line, "Wrong line for %s" % str(path))
        gps_assert(cursor.line(), expected_line, "Wrong column for %s" % str(path))


@run_test_driver
def run_test():
    load = dialogs.Gtk_File_Chooser_Dialog()
    yield load.open_and_yield("Load Sarif File")
    load.select_file("gnatsas.sarif")
    yield load.ok()
    yield wait_idle()

    report = dialogs.AnalysisReport()
    yield report.open_and_yield()

    expected = [
        "Infer (2 items in 2 files)",
        [
            "foo.adb (1 item)",
            [
                "<b>23:4</b>      Memory dynamically allocated by `new` on line 21 is not freed after the last access at line 23, column 4",
                [
                    "          allocation part of the trace starts here",
                    "             allocated by `new` here",
                    "          memory becomes unreachable here",
                ],
            ],
            "does_not_exist.adb (1 item)",
            [
                "<b>23:4</b>      Memory dynamically allocated by `new` on line 21 is not freed after the last access at line 23, column 4",
            ],
        ],
    ]
    gps_assert(dump_locations_tree(), expected, "wrong messages")

    gps_assert(
        report.dump_filters(dialogs.AnalysisReport.FilterKind.TOOL),
        [["Infer", "2", True]],
        "Wrong list of Tools",
    )

    # Test the navigation for 4 rows in the Locations view:
    # - On a primary message
    # - On a secondary message
    # - On a file row for a message with the file not on the disk
    # - On a message for a file not on the disk
    locations = GPS.MDI.get("Locations")
    tree = get_widgets_by_type(Gtk.TreeView, locations.pywidget())[0]
    tree.expand_row(Gtk.TreePath("0:1"), open_all=False)
    yield wait_idle()
    yield test_navigation(tree, "0:0:0", "foo.adb", 23, 4, False)
    yield test_navigation(tree, "0:0:0:0", "foo.adb", 21, 1, False)
    # Need a double click to open a file row
    yield test_navigation(tree, "0:1", "Locations", 1, 1, True)
    yield test_navigation(tree, "0:1:0", "Locations", 1, 1, False)
