"""
Test that the disabled `Show type` does not break the outline view
"""

from GPS import *
from gs_utils.internal.utils import *
from os.path import basename

expected_out_1 = [
    "PkgBodyWithSpec",
    [
        'four <span foreground="#A0A0A0"> return boolean</span>',
        "one",
        "outline_issue",
        ["typeDef_issue"],
        "tree",
        'two <span foreground="#A0A0A0"> return boolean</span>',
    ],
]


@run_test_driver
def run_test():
    GPS.Preference("outline-show-types").set(False)
    yield wait_tasks()
    body = GPS.EditorBuffer.get(GPS.File("pkgbodywithspec.adb"))
    GPS.execute_action("open Outline")
    yield wait_outline("pkgbodywithspec.adb")
    explorer = get_widget_by_name("Outline View Tree")
    gps_assert(
        dump_tree_model(explorer.get_model(), 1),
        expected_out_1,
        "Wrong outline view output",
    )
