"""Check substitutions and argument boundaries, including repeated launches."""

import os
import sys

import GPS
from gs_utils.internal.utils import gps_assert, run_test_driver, wait_idle

sys.path.insert(0, os.path.join(os.environ["GPS_SRC_DIR"], "testsuite", "support"))
from external_editor import FakeEditors, wait_for


@run_test_driver
def driver():
    with FakeEditors() as editors:
        editor = editors.executable("editor")
        editors.select("Custom", '"' + editor + '" "%f" %l %c "%p" %% "two words"')
        for index, (line, column) in enumerate([(3, 4), (7, 2)], start=1):
            filename = editors.open_file(
                "source %d with spaces.txt" % index, line, column
            )
            yield wait_idle()
            editors.invoke()
            if not (
                yield wait_for(
                    lambda: len(editors.records("editor")) == index,
                    "Missing editor invocation %d" % index,
                )
            ):
                return
            expected = [
                filename,
                str(line),
                str(column),
                GPS.Project.root().name(),
                "%",
                "two words",
            ]
            matching = [
                r for r in editors.records("editor") if r["argv"][0] == filename
            ]
            gps_assert(len(matching), 1, "Each file should be opened exactly once")
            if matching:
                gps_assert(matching[0]["argv"], expected, "Incorrect editor arguments")
