"""Successful client commands must not start a server or raise exceptions."""

import os
import sys

from gs_utils.internal.utils import gps_assert, run_test_driver, wait_idle

sys.path.insert(0, os.path.join(os.environ["GPS_SRC_DIR"], "testsuite", "support"))
from external_editor import FakeEditors, wait_for


@run_test_driver
def driver():
    with FakeEditors() as editors:
        for name in ("emacsclient", "gnuclient", "gnudoit", "emacs", "glide"):
            editors.executable(name)
        editors.open_file()
        yield wait_idle()
        editors.isolate_path()

        # The testsuite enables *.EXCEPTIONS and treats unexpected trace
        # output as a failure. This also catches a Program_Error raised after
        # a successful client call; the action API does not expose exit status.
        for client, executable in (
            ("Emacsclient", "emacsclient"),
            ("Gnuclient", "gnuclient"),
        ):
            editors.select(client)
            for count in range(1, 4):
                editors.invoke()
                yield wait_for(
                    lambda: len(editors.records(executable)) == count,
                    "Missing successful " + executable + " invocation",
                )
        yield wait_idle()
        gps_assert(editors.records("emacs"), [], "Unnecessary Emacs server launch")
        gps_assert(editors.records("glide"), [], "Unnecessary Glide server launch")
