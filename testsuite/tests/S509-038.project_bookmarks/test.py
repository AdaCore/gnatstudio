"""
This test verifies that the project bookmarks are saved and restored
"""

import GPS
import os.path
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    c = GPS.EditorBuffer.get(GPS.File('c.adb'))
    c.current_view().goto(c.at(5, 1))
    GPS.execute_action('bookmark create')

    a = GPS.EditorBuffer.get(GPS.File('d.adb'))
    a.current_view().goto(a.at(7, 1))
    GPS.execute_action('project bookmark create')
    yield wait_idle()

    # Get the Bookmarks view
    mdi = GPS.MDI.get("Bookmarks")
    tree = pygps.get_widgets_by_type(Gtk.TreeView, mdi.pywidget())[0]
    model = tree.get_model()
    gps_assert(dump_tree_model(model, 1),
               ['Default:Put (d.adb:7:1)',
                'Get (c.adb:5:1)'],
               "Invalid initial contents of bookmarks")

    project = GPS.Project.load("second.gpr")
    yield wait_idle()
    mdi = GPS.MDI.get("Bookmarks")
    tree = pygps.get_widgets_by_type(Gtk.TreeView, mdi.pywidget())[0]
    model = tree.get_model()
    gps_assert(dump_tree_model(model, 1),
               ['Get (c.adb:5:1)'],
               "Invalid contents of bookmarks for the second project")

    project = GPS.Project.load("default.gpr")
    yield wait_idle()
    mdi = GPS.MDI.get("Bookmarks")
    tree = pygps.get_widgets_by_type(Gtk.TreeView, mdi.pywidget())[0]
    model = tree.get_model()
    gps_assert(dump_tree_model(model, 1),
               ['Default:Put (d.adb:7:1)',
                'Get (c.adb:5:1)'],
               "Invalid contents of bookmarks after a project reloading")
