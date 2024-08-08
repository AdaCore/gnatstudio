"""
Load a project with a missing source directory.
The omnisearch should not raise an exception.
"""

from gs_utils.internal.utils import *


@run_test_driver
def driver():
    GPS.execute_action("Global Search in context: file names")
    yield wait_tasks(other_than=known_tasks)

    field = get_widget_by_name("global_search")
    field.set_text("a")
    yield wait_idle()
    yield timeout(500)

    popup = pygps.get_widget_by_name("global_search-results-list")
    results_tree = pygps.get_widgets_by_type(Gtk.TreeView, popup)[0]
    gps_assert(
        len(dump_tree_model(results_tree.get_model(), 0)) > 3,
        True,
        "Missing files in omnisearch",
    )
