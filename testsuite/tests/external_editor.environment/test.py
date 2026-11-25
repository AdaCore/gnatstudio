"""Editors receive the saved user environment, while the IDE keeps its own."""

import os
import sys

import GPS
from gs_utils.internal.utils import gps_assert, run_test_driver, wait_idle

sys.path.insert(0, os.path.join(os.environ["GPS_SRC_DIR"], "testsuite", "support"))
from external_editor import FakeEditors, wait_for


@run_test_driver
def driver():
    expected = {
        "XDG_DATA_DIRS": os.environ["GPS_STARTUP_XDG_DATA_DIRS"],
        "EXTERNAL_EDITOR_SENTINEL": "user value with spaces",
    }
    parent = {name: GPS.getenv(name) for name in expected}
    with FakeEditors() as editors:
        editor = editors.executable("editor")
        editors.executable("emacsclient")
        editors.open_file()
        yield wait_idle()
        editors.isolate_path()

        # Exercise both Start (Custom) and Run (Emacsclient). Changes made
        # after registration must not leak into the cached user environment.
        editors.setenv("EXTERNAL_EDITOR_LATE_OVERRIDE", "IDE only")
        for client, executable in (
            ("Custom", "editor"),
            ("Emacsclient", "emacsclient"),
        ):
            editors.select(client, '"' + editor + '" %f')
            editors.invoke()
            if not (
                yield wait_for(
                    lambda: editors.records(executable), "Editor not started"
                )
            ):
                return
            child = editors.records(executable)[0]["environment"]
            for name, value in expected.items():
                gps_assert(child.get(name), value, "Saved user environment: " + name)
                gps_assert(
                    GPS.getenv(name),
                    parent[name],
                    "Parent environment changed: " + name,
                )
            for name in (
                "EXTERNAL_EDITOR_ABSENT",
                "EXTERNAL_EDITOR_LATE_OVERRIDE",
                "GPS_STARTUP_XDG_DATA_DIRS",
            ):
                gps_assert(name in child, False, "IDE-only variable inherited: " + name)
        gps_assert(
            GPS.getenv("EXTERNAL_EDITOR_ABSENT"), "IDE only", "Parent variable removed"
        )
