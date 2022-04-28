"""
Interrupt the ALS indexing: no error should be reported
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    def stop_indexing():
        for task in GPS.Task.list():
            if task.label() == "Indexing":
                task.interrupt()
                return False
        return True
    while stop_indexing():
        yield timeout(50)

    # No error should be reported after waiting for the next Progress_Report
    yield timeout(500)
