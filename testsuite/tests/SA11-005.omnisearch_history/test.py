"""
Tests whether the omnisearch history provider works
"""

from gs_utils.internal.utils import (
    run_test_driver,
    gps_assert,
    wait_idle,
    timeout,
    click_in_tree,
    simple_error,
    wait_tasks,
    dump_tree_model,
    find_in_tree,
)


@run_test_driver
def driver():
    # type 'foo' in the omnisearch
    yield wait_tasks()
    w = pygps.get_widget_by_name("global_search")
    w.get_toplevel().grab_focus()
    yield wait_idle()
    w.grab_focus()
    yield wait_idle()
    w.set_text("foo")
    yield timeout(1000)
    popup = pygps.get_widget_by_name("global_search-results-list")
    results_tree = pygps.get_widgets_by_type(Gtk.TreeView, popup)[0]

    # select "foo.adb" file
    click_in_tree(results_tree, path="0", column=1)
    yield timeout(1000)
    if not GPS.EditorBuffer.get().file().name().endswith("foo.adb"):
        simple_error("foo.adb is not opened")

    #  retype 'foo' in the omnisearch
    w.get_toplevel().grab_focus()
    yield wait_idle()
    w.grab_focus()
    yield wait_idle()
    w.set_text("foo")
    yield timeout(1000)

    # check whether we have result from the History provider
    popup = pygps.get_widget_by_name("global_search-results-list")
    results_tree = pygps.get_widgets_by_type(Gtk.TreeView, popup)[0]
    result = dump_tree_model(results_tree.get_model(), 3)

    gps_assert("History (1)" in result, True, "wrong omnisearch result")
