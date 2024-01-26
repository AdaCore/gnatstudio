"""
Directories should not be shown in omnisearch filenames provider.
"""

from gs_utils.internal.utils import *


def get_paths(expected):
    return [GPS.File(e).path for e in expected]


def extract_filename(results):
    extracted = []
    for r in results:
        extracted.append(r.split("<small>")[-1].replace("</small>", ""))
    return extracted


@run_test_driver
def driver():
    GPS.execute_action("Global Search in context: file names")
    yield wait_tasks(other_than=known_tasks)

    # Check that the omnisearch entry has received the focus
    # and that there is no conflict with the editor actions

    field = get_widget_by_name("global_search")
    field.set_text("file_")
    yield wait_idle()
    yield timeout(500)

    popup = pygps.get_widget_by_name('global_search-results-list')
    results_tree = pygps.get_widgets_by_type(Gtk.TreeView, popup)[0]
    gps_assert(get_paths(["file_a.adb", "file_b.adb"]),
               extract_filename(dump_tree_model(results_tree.get_model(), 0)),
               "issue with omnisearch results")
