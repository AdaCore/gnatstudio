"""
Test searching in the project view in an aggregate project
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    explorer = get_widget_by_name("Project Explorer Tree")
    explorer.grab_focus()
    yield wait_idle()

    Filter = get_widget_by_name("Project Explorer Filter")
    Filter.set_text("main")
    yield timeout(500)

    dump = dump_tree_model(explorer.get_model(), 1)
    gps_assert(dump, ['whole', ['array_viewing', 'Basic_Goto']],
               "Incorrect filtering")
