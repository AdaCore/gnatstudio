"""Missing Vim and gnudoit must cause Auto to continue to another client."""

import os
import sys

from gs_utils.internal.utils import gps_assert, run_test_driver, wait_idle

sys.path.insert(0, os.path.join(os.environ["GPS_SRC_DIR"], "testsuite", "support"))
from external_editor import FakeEditors, wait_for


@run_test_driver
def driver():
    with FakeEditors() as editors:
        editors.executable("xterm")
        editors.executable("vi")
        filename = editors.open_file()
        yield wait_idle()
        editors.isolate_path()
        editors.select("Auto")
        editors.invoke()
        if (yield wait_for(lambda: editors.records("xterm"), "Terminal not started")):
            args = editors.records("xterm")[0]["argv"]
            gps_assert("vi" in args, True, "Auto should skip missing Vim and select Vi")
            gps_assert("vim" in args, False, "Auto selected missing Vim")
            gps_assert(args[-1], filename, "Selected editor received the wrong file")

    with FakeEditors() as editors:
        editors.executable("gnuclient")
        fallback = editors.executable("fallback")
        editors.open_file()
        yield wait_idle()
        editors.isolate_path()
        editors.select("Auto", '"' + fallback + '" %f')
        editors.invoke()
        yield wait_for(lambda: editors.records("fallback"), "Auto did not reach Custom")
        gps_assert(
            editors.records("gnuclient"), [], "Auto selected gnuclient without gnudoit"
        )
