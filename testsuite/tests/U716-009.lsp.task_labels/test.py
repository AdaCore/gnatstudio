"""
This test checks that LSP requests associated tasks are
correctly displayed in the tasks HUD widget.
"""
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    b = GPS.EditorBuffer.get(GPS.File("m.adb"))
    b.select(GPS.EditorLocation(b, 6, 45), GPS.EditorLocation(b, 6, 45))
    yield wait_tasks()
    GPS.execute_action("Find All References")

    tasks_labels = [task.label() for task in GPS.Task.list()]
    gps_assert("[Ada] querying references" in tasks_labels, True,
               "Wrong task label after 'find all references': %s" % str(tasks_labels))
