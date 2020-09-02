"""
This test verifies that the project view filters runtime files too.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.Preference("explorer-show-runtime").set(True)
    prj_view = Project_View()
    yield prj_view.open_and_yield()

    project_tree = prj_view.dialog
    path = find_in_tree(project_tree, column=1, key='runtime')
    project_tree.expand_row(path, open_all=True)

    filt = get_widget_by_name("Project Explorer Filter")
    filt.set_text("assert")
    yield wait_idle()

    dump = dump_tree_model(project_tree.get_model(), 1)
    gps_assert(dump[1][1],
               ['a-assert.adb', 'a-assert.ads',
                's-assert.adb', 's-assert.ads'],
               "Project view content wrong after filtering")
