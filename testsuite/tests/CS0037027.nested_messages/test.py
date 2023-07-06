"""Test GPS.Message.get_childrens works """
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    GPS.BuildTarget("Build All").execute(force=True)
    yield wait_tasks()
    
    gps_assert('Constraint_Error' in GPS.Message.list()[0].get_children()[0].get_text(), True)


