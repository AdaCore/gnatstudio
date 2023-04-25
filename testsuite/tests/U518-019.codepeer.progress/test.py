import GPS
from gs_utils.internal.utils import *

"""
This test monitors the progress bar of "Run CodePeer" and verify that its
properly updated during a run.
"""


@run_test_driver
def test_driver():

    # Wait for unrelated tasks
    yield wait_tasks()

    def on_changed(self, progress):
        for t in GPS.Task.list():
            if t.name().startswith("CPL Run CodePeer"):
                cur, total = t.progress()
                if self.prev < cur:
                    self.prev = cur
                    self.cpt += 1

    # Get the Task progress bar and monitor the fraction property
    task_hud_progress = get_widget_by_name("task_hud_progress")
    task_hud_progress.prev = 0  # Previous progress value
    task_hud_progress.cpt = 0   # The number of time the fraction was updated
    task_hud_progress.connect("notify::fraction", on_changed)
    GPS.execute_action("/CodePeer/Analyze All")
    yield wait_tasks()
    gps_assert(task_hud_progress.cpt > 0, True, "Wrong number of step")
