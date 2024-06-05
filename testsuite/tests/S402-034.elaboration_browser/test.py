"""
This test checks Elaboration Circularities view
"""
import GPS
from gs_utils.internal.utils import gps_assert, hook, run_test_driver, wait_idle


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()
    GPS.execute_action("Build Main Number 1")
    yield hook("compilation_finished")
    yield wait_idle()

    gps_assert(
        GPS.dump_elaborations(),
        ["b (body):a (spec):pragma Elaborate_All", "a (body):b (spec):withed"],
        "The elaboration Ccrcularities view has wrong content",
    )
