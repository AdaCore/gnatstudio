"""
This test checks that we display a message in the Outline when there
are no symbols available.
"""

import GPS
from gs_utils.internal.utils import *


expected_1 = [' <span foreground="#A0A0A0">No symbols available</span>']

expected_2 = ['Test', ['Do_Nothing', 'A']]


@run_test_driver
def run_test():
    GPS.execute_action("open Outline")

    buf =GPS.EditorBuffer.get(GPS.File("test.adb"))
    yield wait_language_server("textDocument/documentSymbol", "Ada")
    yield wait_idle()

    # Check the Outline view contents
    explorer = get_widget_by_name("Outline View Tree")
    GPS.Console().write(str(dump_tree_model(explorer.get_model(), 1)))
    gps_assert(dump_tree_model(explorer.get_model(), 1),
               expected_1,
               "Wrong outline view for test.adb")

    buf =GPS.EditorBuffer.get(GPS.File("test.ads"))
    yield wait_language_server("textDocument/documentSymbol", "Ada")
    yield wait_idle()
    GPS.Console().write(str(dump_tree_model(explorer.get_model(), 1)))

    gps_assert(dump_tree_model(explorer.get_model(), 1),
               expected_2,
               "Wrong outline view for test.ads")
