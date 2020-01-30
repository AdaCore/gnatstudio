"""
This test checks that executing completion actions while the completion
window is opened selects the next item in the completion list.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Insert My_Var in main.adb

    editor = GPS.EditorBuffer.get(GPS.File("main.adb"))
    start_loc = editor.at(6, 1)
    editor.insert(start_loc, "My_Var")

    GPS.execute_action("Complete identifier (advanced)")

    tree = get_widget_by_name("completion-view")
    selected_path = tree.get_selection().get_selected_rows()[1][0].to_string()

    gps_assert(selected_path, "0",
               "The first item should be selected when starting completion")

    GPS.execute_action("Complete identifier (advanced)")

    selected_path = tree.get_selection().get_selected_rows()[1][0].to_string()

    gps_assert(selected_path, "1",
               "The second item should be selected when executing "
               + "'complete identifier' while the completion window is opened")
