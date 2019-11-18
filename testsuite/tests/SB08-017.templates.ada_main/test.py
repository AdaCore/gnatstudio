"""
This test check that we correctly use the new file's base name
when adding a new main unit via the 'new ada main unit' action.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test():
    # Click in the 'src' directory in the Project view
    GPS.MDI.get("Project").raise_window()
    explorer = get_widget_by_name("Project Explorer Tree")
    select_in_tree(explorer, column=1, key='src')
    yield wait_idle()

    # try to create a new Ada main unit from there
    yield idle_modal_dialog(
        lambda: GPS.execute_action('new ada main unit'))
    dialog = get_window_by_title("Create Ada Main Unit")

    # Create a unit 'my.unit'
    name_ent = get_widgets_by_type(Gtk.Entry, dialog)[0]
    name_ent.set_text("my.unit")

    ok_button = get_button_from_label("OK", dialog)
    yield idle_modal_dialog(lambda: ok_button.clicked())

    # Confirmation that we want to add this new unit as
    # a main for the loaded project.
    dialog = get_window_by_title("Confirmation")
    get_stock_button(dialog, Gtk.STOCK_YES).clicked()

    yield hook('project_view_changed')
    mains = GPS.Project.root().get_attribute_as_list("main")

    gps_assert(mains, ["my-unit.adb"],
               "New Ada main was not correctly added")
