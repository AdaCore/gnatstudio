"""
This test checks that the Outline view works fine in LSP mode from the
Project view, even when there is no opened editor for the selected file.
"""

import GPS
from gs_utils.internal.utils import *


expected =  ['My_Class',
             ['My_Class',
              'do_something',
              'ch',
              'num']]


@run_test_driver
def run_test():
    GPS.execute_action("open Outline")

    # Select 'my_class.hh' directory in the Project view
    GPS.MDI.get("Project").raise_window()
    explorer = get_widget_by_name("Project Explorer Tree")
    select_in_tree(explorer, column=1, key='my_class.hh')

    yield wait_language_server("textDocument/documentSymbol", "C++")
    yield timeout(300)

    # Check the Outline view contents
    explorer = get_widget_by_name("Outline View Tree")
    GPS.Console().write(str(dump_tree_model(explorer.get_model(), 1)))
    gps_assert(dump_tree_model(explorer.get_model(), 1),
               expected,
               "Wrong outline view for main.cpp")
