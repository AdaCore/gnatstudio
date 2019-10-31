"""
Test the actions:
- "Edit local configuration file"
- "Edit global configuration file"
"""

import GPS
from gs_utils.internal.utils import *

NAME_COLUMN = 1


@run_test_driver
def test_driver():
    GPS.execute_action("open Project")
    view = GPS.MDI.get("Project")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    gps_assert(tree is not None, True, "Can't retrieve the Project treeview")
    selection = tree.get_selection()

    # Test on the root project were both of them are properly defined
    select_in_tree(tree, column=NAME_COLUMN, key="Test")
    GPS.execute_action("Edit local configuration file")
    gps_assert(GPS.MDI.get("gnat.adc") is not None,
               True,
               "The local configuration file should be opened")
    # Give the focus back to the Project view, because the action is depending
    # of the project view context
    view.raise_window()
    GPS.execute_action("Edit global configuration file")
    gps_assert(GPS.MDI.get("my_global.foo") is not None,
               True,
               "The global configuration file should be opened")

    # Test on the sub project were the global attribute is not set
    # The view support the multiselection, thus we need to unselect before
    view.raise_window()
    selection.unselect_all()
    select_in_tree(tree, column=NAME_COLUMN, key="Nested")
    GPS.execute_action("Edit local configuration file")
    gps_assert(GPS.MDI.get("nested.adc") is not None,
               True,
               "Can't open the local configuration for a subproject")
    expected = len(GPS.MDI.children())
    view.raise_window()
    GPS.execute_action("Edit global configuration file")
    gps_assert(len(GPS.MDI.children()),
               expected,
               "The attribute is not defined: no new editor should be opened")
    gps_assert("is not defined in " in GPS.Console("Messages").get_text(),
               True,
               "Missing message in the console")
