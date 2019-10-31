"""
Verify the Project view is correclty sorted.
"""

from GPS import *
from gs_utils.internal.utils import *

expected = ['Default',
            ['.',
             ['a.adb',
              'a.ads',
              'a-a.ads',
              'a-b.ads',
              'aa.ads',
              'ab.adb',
              'ab.ads',
              'b.adb',
              'b.ads'],
             '.']]


@run_test_driver
def run_test():
    GPS.execute_action("open Project")
    explorer = get_widget_by_name("Project Explorer Tree")
    yield wait_tasks(other_than=known_tasks)
    gps_assert(expected,
               dump_tree_model(explorer.get_model(), 1),
               "Issue in the Project view sort")
