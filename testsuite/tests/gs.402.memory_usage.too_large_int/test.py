"""
This test checks that the ld plugin handles unknown memory regions
in the generated map file.
"""

from time import time
import GPS
from gs_utils.internal.utils import *

PARSING_ERROR_MSG = "Exception while parsing map file:"


@run_test_driver
def run_test():
    # Open the Memory Usage View
    GPS.execute_action("open Memory Usage")
    yield wait_for_mdi_child("Memory Usage")

    # Get Memory Usage view's model
    memory_usage_view = GPS.MDI.get("Memory Usage").get_child().pywidget()
    tree = get_widgets_by_type(Gtk.TreeView, memory_usage_view)[0]
    tree.expand_all()
    model = tree.get_model()

    # Verify that we have the 'ram' region: this means that we did not
    # get any exception while parsing the map file
    gps_assert(
        model.get_value(model.get_iter_first(), 1),
        "ram",
        "The Memory Usage View is empty while it should not",
    )

    # Check its size too
    gps_assert(
        model.get_value(model.get_iter_first(), 3),
        "2.09 KB / 16 KB",
        "Wrong size computed for the 'ram' region",
    )

    # Check that we don't have any parsing error message in the
    # Messages view
    gps_assert(
        PARSING_ERROR_MSG not in GPS.Console().get_text(),
        True,
        "Parsing error message found in the Messages view",
    )
