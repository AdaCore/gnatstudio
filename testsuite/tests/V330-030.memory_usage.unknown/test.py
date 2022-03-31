"""
This test checks that the ld plugin hanles unknown memory regions
in the generated map file.
"""

from time import time
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

    # Verify that we have the 'default' region...
    gps_assert(model.get_value(model.get_iter_first(), 1), "*default*",
               "The Memory Usage View is empty while it should not")

    #... with an 'unknown' size
    gps_assert(model.get_value(model.get_iter_first(), 3), "2.45 KB / unknown",
               "The Memory Usage View is empty while it should not")