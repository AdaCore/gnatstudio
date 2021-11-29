"""
This test checks that LSP requests associated tasks are
correctly displayed in the tasks HUD widget.
"""
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    yield wait_tasks()
    b = GPS.EditorBuffer.get(GPS.File("m.adb"))
    b.select(GPS.EditorLocation(b, 6, 45), GPS.EditorLocation(b, 6, 45))
    yield timeout(500)
    GPS.execute_action("Find All References")
    task_hud_label = get_widget_by_name("task_hud_label")

    label = ""
    count = 0
    # wait_until_true() has a granularity of 500ms, so it might miss
    # the label update if it's shorter than this granularity. So use
    # a manual loop instead.
    while label == "":
        label = task_hud_label.get_text()
        if "[Ada]" in label:
            break
        count += 1
        if count > 200:
            break
        yield timeout(10)

    gps_assert(label, "[Ada] querying references",
               "Wrong task label after 'find all references'")
