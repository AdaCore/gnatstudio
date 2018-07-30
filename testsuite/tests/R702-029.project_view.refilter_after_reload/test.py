"""Verify that the project view filter is still active after project reload"""

from gps_utils.internal.utils import run_test_driver, get_widget_by_name, \
    dump_tree_model, gps_assert, timeout


@run_test_driver
def driver():
    yield timeout(100)
    explorer = get_widget_by_name("Project Explorer Tree")
    dump = dump_tree_model(explorer.get_model(), 1)
    gps_assert(dump,
               ['p', ['.', ['beau.adb', 'nico.adb'], '.']],
               "Initial project view contents wrong")

    filt = get_widget_by_name("Project Explorer Filter")
    filt.set_text("be")
    yield timeout(100)
    dump = dump_tree_model(explorer.get_model(), 1)
    gps_assert(dump,
               ['p', ['.', ['beau.adb'], '.']],
               "Project view content wrong after filtering")

    GPS.execute_action("reload project")
    dump = dump_tree_model(explorer.get_model(), 1)
    gps_assert(dump,
               ['p', ['.', ['beau.adb'], '.']],
               "Project view not filtered properly after reload")
