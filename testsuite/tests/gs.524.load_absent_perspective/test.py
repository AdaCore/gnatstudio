"""
Test that we handle focus after absent perspective loading
"""

import GPS
from gs_utils.internal.utils import *


expected = ["Main", ["P", "Test", "Is_Bool"]]


@run_test_driver
def run_test():
    yield wait_tasks()
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    spec = GPS.EditorBuffer.get(GPS.File("pkgbodywithspec.ads"))
    GPS.execute_action("open Outline")
    yield wait_idle()

    GPS.MDI.load_perspective("absent")
    yield wait_idle()

    GPS.MDI.get("main.adb").raise_window()
    yield wait_outline("main.adb")

    explorer = get_widget_by_name("Outline View Tree")
    gps_assert(
        dump_tree_model(explorer.get_model(), 1),
        expected,
        "Wrong outline view output",
    )
