"""
This test checks that the Memory Usage View is correctly filled
if a linker map file is already present when the project view changes
(e.g: when loading the project).
"""

import GPS
from gs_utils.internal.utils import *

@run_test_driver
def run_test():
    # Open the Memory Usage View
    GPS.execute_action('open Memory Usage')
    yield wait_for_mdi_child('Memory Usage')

    # Get its associated tree model
    memory_usage_view = GPS.MDI.get('Memory Usage').get_child().pywidget()
    tree = get_widgets_by_type(Gtk.TreeView, memory_usage_view)[0]
    model = tree.get_model()

    # Verify that it's not empty
    gps_assert(len(dump_tree_model(model, 0)) != 0, True,
               "The Memory Usage View is empty while it should not")
