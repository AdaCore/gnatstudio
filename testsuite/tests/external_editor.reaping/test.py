"""A child that exits after launch must be reaped by the IDE automatically."""

import os
import sys
from pathlib import Path

from gs_utils.internal.utils import run_test_driver, wait_idle

sys.path.insert(0, os.path.join(os.environ["GPS_SRC_DIR"], "testsuite", "support"))
from external_editor import FakeEditors, wait_for


@run_test_driver
def driver():
    with FakeEditors() as editors:
        editor = editors.executable("editor", hold=True)
        editors.select("Custom", '"' + editor + '" %f')
        editors.open_file()
        yield wait_idle()
        editors.invoke()
        if not (
            yield wait_for(lambda: editors.records("editor"), "Editor not started")
        ):
            return
        pid = editors.records("editor")[0]["pid"]
        editors.release("editor")
        if not (
            yield wait_for(lambda: editors.finished("editor"), "Editor did not finish")
        ):
            return
        # Do not call waitpid or launch another editor: either would hide the
        # missing periodic cleanup. A zombie still has a /proc/<pid> entry.
        yield wait_for(
            lambda: not Path("/proc", str(pid)).exists(),
            "External editor was not reaped after exiting",
        )
