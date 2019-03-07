"""
This test verifies that the 'delete directory' action
does not try to remove non-existent source directories.
"""
import GPS
import os.path
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    prj_view = Project_View()
    yield prj_view.open_and_yield()

    prj_view.select_by_name(column=1, value="src")

    yield idle_modal_dialog(lambda: GPS.execute_action("delete directory"))

    dialog = get_window_by_title("Confirmation", Gtk.Window.list_toplevels())
    gps_assert(dialog is not None, True,
               "The 'delete file' dialog should have been opened")
    button = get_button_from_label("Yes", dialog)
    button.clicked()

    yield wait_tasks(other_than=known_tasks)

    gps_assert("Cannot remove directory: " in GPS.Console().get_text(),
               True,
               "An error message stating that we can't remove the directory "
               + "should be displayed")

    gps_assert(os.path.isfile(GPS.File("main.adb").path), True,
               "Parent directory files have been removed")
