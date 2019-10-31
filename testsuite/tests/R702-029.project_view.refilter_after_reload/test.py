"""Verify that the project view filter is still active after project reload"""

from gs_utils.internal.utils import run_test_driver, get_widget_by_name, \
    dump_tree_model, gps_assert, timeout, wait_idle, wait_tasks
from gs_utils.internal.dialogs import Project_View


@run_test_driver
def driver():
    prj_view = Project_View()
    yield prj_view.open_and_yield()

    explorer = prj_view.dialog
    dump = dump_tree_model(explorer.get_model(), 1)
    gps_assert(dump,
               ['p', ['.', ['beau.adb', 'nico.adb'], '.']],
               "Initial project view contents wrong")

    filt = get_widget_by_name("Project Explorer Filter")
    filt.set_text("be")
    yield wait_tasks()
    dump = dump_tree_model(explorer.get_model(), 1)
    gps_assert(dump,
               ['p', ['.', ['beau.adb'], '.']],
               "Project view content wrong after filtering")

    GPS.execute_action("reload project")
    yield wait_tasks()
    dump = dump_tree_model(explorer.get_model(), 1)
    gps_assert(dump,
               ['p', ['.', ['beau.adb'], '.']],
               "Project view not filtered properly after reload")
