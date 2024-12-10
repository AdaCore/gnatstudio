"""
Test for the 'Create bug report' dialog.
"""
import GPS
from gs_utils.internal.utils import *
import os
import os.path


@run_test_driver
def run_test():
    yield wait_tasks()
    GPS.execute_action("Create bug report")
    dialog = get_widget_by_name("gps-bug-report-dialog")

    # Verify that the 'show' link buttons work by opening the preferences file
    show_buttons = get_widgets_by_type(Gtk.LinkButton, dialog)
    show_prefs_button = show_buttons[1]
    show_prefs_button.clicked()

    gps_assert(
        GPS.EditorBuffer.get().file().base_name(),
        "preferences_copy.xml",
        "Clickin on the 'show' link button should have opened a copy"
        + " of preferences.xml",
    )

    # Only include the ALS log file
    check_buttons = get_widgets_by_type(Gtk.CheckButton, dialog)
    check_buttons[1].set_active(False)

    # Set a custom root path for the bug reports
    path_entry = get_widgets_by_type(Gtk.Entry, dialog)[0]
    custom_path = os.path.join(GPS.pwd(), "bug_reports")
    path_entry.set_text(custom_path)

    # Click on OK
    ok_button = get_button_from_label("_OK", dialog)
    ok_button.clicked()

    # Verify that the bug report archive has correctly been created
    archive = os.listdir(custom_path)[0]
    gps_assert(
        os.path.exists(archive), True, "The bug report archive has not been created"
    )

    # Untar it and check its contents: it should only contain the ALS
    # log
    GPS.cd(custom_path)
    GPS.Process(["tar", "-xf", archive]).get_result()

    contents = os.listdir(archive.replace(".tar.bz2", ""))
    gps_assert(
        contents, ["inout.txt"], "The bug report should only contain the ALS log"
    )
