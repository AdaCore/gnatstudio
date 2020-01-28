"""Test GPS.BuildTarget("Build All").get_expanded_command_line()"""
from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    yield wait_tasks(other_than=known_tasks)
    gps_assert(GPS.BuildTarget("Build All").get_expanded_command_line(),
               ['gprbuild',
                '-d',
                '-P' + GPS.Project.root().file().name()],
               "Wrong expanded command line")
