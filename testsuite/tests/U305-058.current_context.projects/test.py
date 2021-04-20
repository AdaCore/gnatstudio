"""
Test that GPS.current_context().projects() works
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    explorer = get_widget_by_name("Project Explorer Tree")
    explorer.grab_focus()

    explorer.expand_row(Gtk.TreePath((0,0)), open_all=False)
    explorer.expand_row(Gtk.TreePath((0,2)), open_all=False)
    explorer.expand_row(Gtk.TreePath((0,2,0)), open_all=False)
    explorer.expand_row(Gtk.TreePath((0,3)), open_all=False)
    explorer.expand_row(Gtk.TreePath((0,3,0)), open_all=False)

    path = find_in_tree(explorer, column=1, key="main.adb")
    explorer.get_selection().select_path(path)
    path = find_in_tree(explorer, column=1, key="a.adb")
    explorer.get_selection().select_path(path)
    path = find_in_tree(explorer, column=1, key="b.adb")
    explorer.get_selection().select_path(path)

    yield wait_idle()
    gps_assert(len(GPS.current_context().files()), 3)
    gps_assert(len(GPS.current_context().projects()), 3)
