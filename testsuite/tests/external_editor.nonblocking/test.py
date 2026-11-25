"""An editor can stay open without blocking the action or GTK callbacks."""

import os
import sys
import time

import GPS
from gs_utils.internal.utils import gps_assert, run_test_driver, wait_idle

sys.path.insert(0, os.path.join(os.environ["GPS_SRC_DIR"], "testsuite", "support"))
from external_editor import FakeEditors, wait_for


@run_test_driver
def driver():
    with FakeEditors() as editors:
        editor = editors.executable("editor", hold=True)
        editors.select("Custom", '"' + editor + '" %f')
        editors.open_file()
        yield wait_idle()

        callback_times = []
        started = time.monotonic()

        def tick(timer):
            callback_times.append(time.monotonic() - started)
            timer.remove()

        timer = GPS.Timeout(100, tick)
        try:
            editors.invoke()
            elapsed = time.monotonic() - started
            # Allow scheduling slack, but detect the five-second process wait.
            gps_assert(elapsed < 3, True, "Launching an editor blocked the action")
            yield wait_for(lambda: callback_times, "GTK callback never ran")
            if callback_times:
                gps_assert(
                    callback_times[0] < 3, True, "Launching an editor blocked GTK"
                )
            yield wait_for(lambda: editors.records("editor"), "Editor not started")
            gps_assert(editors.finished("editor"), False, "Editor should still be open")
        finally:
            if not callback_times:
                timer.remove()
            editors.release("editor")
