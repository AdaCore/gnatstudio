"""
This test checks that the ld plugin for the Memory Usage View is 
correctly enabled when assembly is specified in the project's languages
"""

import GPS
from gs_utils.internal.utils import *

@run_test_driver
def run_test():
    # Open the Memory Usage View
    GPS.execute_action('open Memory Usage')
    yield wait_for_mdi_child('Memory Usage')

    # Get Memory Usage view's model
    memory_usage_view = GPS.MDI.get('Memory Usage').get_child().pywidget()
    tree = get_widgets_by_type(Gtk.TreeView, memory_usage_view)[0]
    tree.expand_all()
    model = tree.get_model()

    # GPS.Console().write(str(dump_tree_model(model)))

    # Verify that we have the 'DDR' region...
    gps_assert(model.get_value(model.get_iter_first(), 1), "DDR",
               "The Memory Usage View is empty while it should not")

    #... with the correct size
    gps_assert(model.get_value(model.get_iter_first(), 3), "312.73 MB / 2 GB",
               "Wrong size specified for the DDR memory region")
