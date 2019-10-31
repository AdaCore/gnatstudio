"""Test outline view for expression function containing case expression"""
from GPS import *
from gs_utils.internal.utils import *

expected_out_1 = \
    ['Main',
     ['Bbb',
      'Hello <span foreground="#A0A0A0"> return String</span>']]

@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("open Outline")
    yield wait_tasks(other_than=known_tasks)
    explorer = get_widget_by_name("Outline View Tree")
    gps_assert(dump_tree_model(explorer.get_model(), 1),
               expected_out_1,
               "Wrong outline view output")
