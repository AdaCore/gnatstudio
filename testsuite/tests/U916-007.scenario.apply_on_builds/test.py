"""
This test checks that we display a confirmation dialog asking the
user if he wants to apply changes regarding scenario variables
before building.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.execute_action("open Scenario")
    view = GPS.MDI.get("Scenario").pywidget()

    # Get the 'Bla' scenario variable combobox
    var_flowbox = get_widget_by_name("Bla", view)
    combo = get_widgets_by_type(Gtk.ComboBox, var_flowbox)[0]

    gps_assert(combo.get_active_text(), "a",
               "We should have 'a' for the scenario variable initially")

    # Set the scenario variable to 'b', without applying the changes
    combo.set_active(1)

    # Launch a build: we should get a confirmation dialog
    yield idle_modal_dialog(
        lambda: GPS.execute_action("Build & Run Number 1"))
    dialog = get_window_by_title("Confirmation")
    yes_button = get_button_from_label("Yes", dialog)
    yes_button.clicked()
    yield wait_tasks()

    # Check that the scenario variable has been put to 'b' when building
    gps_assert("-XBla=b" in GPS.Console().get_text(), True,
               "The scenario variable should have been updated when building")
