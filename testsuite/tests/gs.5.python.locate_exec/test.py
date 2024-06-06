"""
This test checks that locate_exec_on_path returns the file
extensions.
"""

import os
import os.path
import os_utils
from gs_utils.internal.utils import run_test_driver, gps_assert


@run_test_driver
def driver():
    if os.name == "nt":
        expected = "gnat.exe"
    else:
        expected = "gnat"

    gps_assert(
        os.path.basename(os_utils.locate_exec_on_path("gnat")),
        expected,
        "Wrong executable",
    )
