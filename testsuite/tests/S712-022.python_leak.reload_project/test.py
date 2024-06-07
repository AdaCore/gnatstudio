"""
Verify that a VCS2 engine are freed between project reload
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
    yield wait_tasks()
    expected = count_object("File")
    # Compare to "reload project" this cause a hard reload which resets the
    # VCS engine.
    GPS.execute_action("/File/Open Recent Projects/default.gpr")
    yield wait_tasks()
    # Manually trigger the garbage collector: most of the memory can be freed
    # however the threshold was not meet thus the gc was not triggered
    # automatically.
    gc.collect()

    gps_assert(count_object("Git"), 1, "Reloading should freed the previous git engine")
    gps_assert(count_object("File"), expected, "Reloading should not leak files")
