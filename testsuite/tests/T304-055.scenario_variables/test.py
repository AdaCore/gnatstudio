'''
Verifying that scenario_variables returns untyped variables
'''

from gs_utils.internal.utils import *

@run_test_driver
def driver():
    yield wait_tasks()
    gps_assert(
        GPS.Project.scenario_variables(),
        {'Foo': 'bar',
         'GPS_COMPIL_ARGS': '../'},
        "Wrong result")
