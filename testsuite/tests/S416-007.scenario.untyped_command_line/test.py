"""
The untyped vars should be expanded with %X
"""

import GPS
from gps_utils.internal.utils import *


@run_test_driver
def test_driver():
    gps_assert(GPS.Project.scenario_variables_cmd_line(),
               "Ext1=Foo Ext2=Hello_World! ",
               "Fails to expand %X")
