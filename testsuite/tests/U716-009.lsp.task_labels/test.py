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
    yield wait_until_true(lambda: task_hud_label.get_text() != "")
    gps_assert(task_hud_label.get_text(), "[Ada] querying references",
               "Wrong task label after 'find all references'")
    yield wait_language_server("textDocument/references")
