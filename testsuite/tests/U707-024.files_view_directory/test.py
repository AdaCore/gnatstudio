"""
This test checks that directory selection works for the files view
"""
import GPS
from gs_utils.internal.utils import (
    gps_assert,
    run_test_driver,
    wait_idle,
    get_widget_by_name,
    dump_tree_model,
    wait_tasks,
)
import os


@run_test_driver
def run_test():
    GPS.execute_action("open Files")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()
    entry = get_widget_by_name("Files_View_Directory")
    entry.set_text(os.path.join(GPS.Project.root().file().directory(), "b"))
    yield wait_tasks()

    explorer = get_widget_by_name("File Explorer Tree")
    d = dump_tree_model(explorer.get_model(), 1)
    gps_assert(d, ["b", ["b.adb", "b.ads"]], "Wrong contents of the files view")
