"""
Verify that the we correctly preserve the nodes that are expanded
when filtering the Project view.
"""

from gs_utils.internal.utils import run_test_driver, get_widget_by_name, \
    dump_tree_model, gps_assert, timeout, wait_idle, wait_tasks
from gs_utils.internal.dialogs import Project_View


@run_test_driver
def driver():
    prj_view = Project_View()
    yield prj_view.open_and_yield()

    explorer = prj_view.dialog
    explorer.expand_all()

    filt = get_widget_by_name("Project Explorer Filter")
    filt.set_text("be")
    yield wait_tasks()
    dump = dump_tree_model(explorer.get_model(), 1)
    gps_assert(dump,
               ['p', ['src1', ['beau.adb']]],
               "Project view content wrong after filtering")

    filt.set_text("")
    yield wait_tasks()
    dump = dump_tree_model(explorer.get_model(), 1)
    GPS.Console().write(str(dump))
    gps_assert(dump,
               ['p', ['src1', ['beau.adb'], 'src2', ['nico.adb'], '.']],
               "Project view content wrong after removing the filter text")
