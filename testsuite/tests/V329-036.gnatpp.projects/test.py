"""
This test checks that gnatpp actions and project and subprojects
work correctly.
"""
import GPS
from workflows import run_as_workflow
from gs_utils.internal.utils import *


@run_as_workflow
def run_gnatpp(unit_to_check, subprojects=False, excluded_unit=None):
    action_name = "run gnatpp on project"

    if subprojects:
        action_name += " and subprojects"

    yield idle_modal_dialog(
        lambda: GPS.execute_action(action_name))
    dialog = get_window_by_title("Pretty Print current project")
    get_button_from_label("Execute", dialog).clicked()
    yield wait_tasks()

    text = GPS.Console().get_text()
    gps_assert(
        "gnatpp" in text and unit_to_check in text,
        True,
        unit_to_check + " should be pretty printed",
    )
    if excluded_unit:
        gps_assert(
        excluded_unit not in text,
        True,
        excluded_unit + " should not be pretty printed",
    )


@run_test_driver
def test_driver():
    prj_view = Project_View()
    yield prj_view.open_and_yield()
    prj_view.select_by_name(column=1, value="Default")
    yield wait_idle()

    # Run gnatpp on the root project only
    yield run_gnatpp(unit_to_check="unit.adb", subprojects=False,
        excluded_unit="main.adb")

    # Run gnatpp on the root project and subprojects
    yield run_gnatpp(unit_to_check="main.adb", subprojects=True)