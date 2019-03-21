"""
This test verifies that the good behavior of the 'Entities for current
file' search provider.
"""


import GPS
from gps_utils.internal.utils import *


def perform_search(expected_count):
    # Search for the 'Count' entity in main.adb and verify
    # that we only get one result

    count = 0
    for result in GPS.Search.search(
            GPS.Search.CURRENT_FILE_ENTITIES,
            "Count",
            GPS.Search.WHOLE_WORD):
        count += 1
    gps_assert(count, expected_count, 
               "Wrong number of search results\n" +
               "expected: %u vs found: %u" % (expected_count, count))


@run_test_driver
def test_driver():
    # Wait for the 'load constructs' task 
    yield wait_tasks()
    
    # Open main.adb and perform the search
    buffer = GPS.EditorBuffer.get(GPS.File("main.adb"))
    perform_search(expected_count=1)
    
    # Close main.adb and verify that we get 0 results since no editor
    # is focused
    GPS.execute_action("close all editors")
    perform_search(expected_count=0)
    

    

