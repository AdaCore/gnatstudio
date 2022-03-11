"""
Spawn multiple GPS.Process shared between Ada and Python (in this case
git commands) and verify they are freed from memory.
"""
import gc
import GPS
from gs_utils.internal.utils import *


def count_object(typename, objects=None):
    if objects is None:
        objects = gc.get_objects()
    return len([o for o in objects if type(o).__name__ == typename])


@run_test_driver
def test_driver():
    # Test multiple process related to git (around 6 of them)
    GPS.execute_action("open Commits")
    yield wait_tasks()
    GPS.execute_action("vcs reload status")
    yield wait_tasks()
    gps_assert(count_object("Process"),
               0,
               "All process should be freed")

    # Test unreferenced process should be freed by gc at the end
    GPS.Process(["sleep", "3"])
    gps_assert(count_object("Process"),
               1,
               "Only one process should be running")
    yield timeout(4000)  # More than the 3s
    gps_assert(count_object("Process"),
               0,
               "Sleep was not referenced and thus should be collected")

    # Test for referenced object in local variable
    p = GPS.Process(["sleep", "0.1"])
    yield timeout(1000)
    gc.collect()
    gps_assert(count_object("Process"),
               1,
               "The process is explicitly referenced and should still exist")
    p = None
    gc.collect()
    gps_assert(count_object("Process"),
               0,
               "The garbage collector missed Process")
