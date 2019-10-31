"""
This test verifies that users can't enter filenames when creating
new source files via file templates.
"""

import GPS
from gs_utils.internal.utils import *
from workflows import run_as_workflow


@run_test_driver
def run_test():
    @run_as_workflow
    def on_create_main_unit_dialog(dialog):
        # Try to enter a filename instead of a valid unit name
        name_ent = get_widgets_by_type(Gtk.Entry, dialog)[0]
        name_ent.set_text("main.adb")

        # Verify that the 'Ok' button is insensitive
        ok_button = get_button_from_label("OK", dialog)
        gps_assert(ok_button.get_sensitive(), False,
                   "The OK button should not be clickable")

        # Now enter a valid unit name
        name_ent.set_text("Main")

        # Verify that the 'Ok' button is now sensitive
        gps_assert(ok_button.get_sensitive(), True,
                   "The OK button should now be clickable")

        # Cancel the dialog
        cancel_button = get_stock_button(dialog, Gtk.STOCK_CANCEL)
        cancel_button.clicked()


    # Click in the 'src' directory in the Project view
    GPS.MDI.get("Project").raise_window()
    explorer = get_widget_by_name("Project Explorer Tree")
    select_in_tree(explorer, column=1, key='.')

    # try to create a new Ada main unit from there
    before_dialog(on_create_main_unit_dialog)
    GPS.execute_action('new ada main unit')

    yield wait_tasks(other_than=known_tasks)
