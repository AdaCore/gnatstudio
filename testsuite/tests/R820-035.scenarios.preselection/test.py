"""
This test verifies that all the scenarios are preselected by default
when opening the Project Properties editor.
"""

from GPS import *
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    e = Project_Properties_Editor()
    yield e.open_and_yield()

    expected = ["VAR_2", ["C", "D"], "VAR_1", ["A", "B"]]

    gps_assert(e.get_scenarios(), expected,
               "All the scenarios should be preselected")
    yield e.save()
