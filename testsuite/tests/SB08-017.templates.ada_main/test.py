"""
This test check that we correctly use the new file's base name
when adding a new main unit via the 'new ada main unit' action.
"""

import GPS
from gs_utils.internal.utils import *
from workflows import run_as_workflow


@run_as_workflow
def create_ada_main(unit_name):
    """Helper to create an Ada main unit"""
    # Click in the 'src' directory in the Project view
    GPS.MDI.get("Project").raise_window()
    explorer = get_widget_by_name("Project Explorer Tree")
    select_in_tree(explorer, column=1, key="src")
    yield wait_idle()

    # Open the dialog to create a new Ada main unit
    yield idle_modal_dialog(lambda: GPS.execute_action("new ada main unit"))
    dialog = get_window_by_title("Create Ada Main Unit")

    # Set the unit name
    name_ent = get_widgets_by_type(Gtk.Entry, dialog)[0]
    name_ent.set_text(unit_name)

    # Click OK
    ok_button = get_button_from_label("OK", dialog)
    yield idle_modal_dialog(lambda: ok_button.clicked())

    # Confirm that we want to add this new unit as a main
    dialog = get_window_by_title("Confirmation")
    get_stock_button(dialog, STOCK_YES).clicked()
    yield hook("project_view_changed")


@run_test_driver
def test():
    # Create a unit 'my.unit', adding blankspaces before and after:
    # they should be stripped at the end.
    yield create_ada_main(" my.unit ")

    mains = GPS.Project.root().get_attribute_as_list("main")
    gps_assert(mains, ["my-unit.adb"], "New Ada main was not correctly added")

    # Now add a second main unit, this time with no spaces
    yield create_ada_main("another_unit")

    mains = GPS.Project.root().get_attribute_as_list("main")
    gps_assert(
        mains,
        ["my-unit.adb", "another_unit.adb"],
        "Second Ada main was not correctly added",
    )

    # Verify that the project file has the focus after the last addition
    current_view = GPS.MDI.current()
    gps_assert(
        current_view.name(short=True),
        "default.gpr",
        "Project file should have the focus after adding a new main unit",
    )
