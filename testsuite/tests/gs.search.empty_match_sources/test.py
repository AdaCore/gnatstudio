"""
Search the sources with a regexp that matches the empty string: the
results must be built without raising an exception, and no highlighting
is applied to a zero-length match.
"""

from gs_utils.internal.utils import *


@run_test_driver
def driver():
    GPS.execute_action("Global Search in context: Sources")
    yield wait_tasks(other_than=known_tasks)

    # Switch the global search to regexp mode
    get_widget_by_name("global_search-kind-REGEXP").set_active(True)
    yield wait_idle()

    # 'X*' matches the empty string at every position of main.adb, which
    # contains no 'X' at all
    field = get_widget_by_name("global_search")
    field.set_text("X*")
    yield wait_idle()
    yield timeout(1000)

    popup = get_widget_by_name("global_search-results-list")
    results_tree = get_widgets_by_type(Gtk.TreeView, popup)[0]
    results = dump_tree_model(results_tree.get_model(), 0)
    gps_assert(len(results) > 0, True, "the empty regexp match produced no result")
