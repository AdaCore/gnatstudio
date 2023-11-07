"""
This test verifies that target reference is taken
into account for project directories
"""
import GPS
import os.path
from gs_utils.internal.utils import *

EXPECTED_TARGET_LINUX = "x86_64-pc-linux-gnu"
EXPECTED_TARGET_WINDOWS = "x86_64-w64-mingw32"


@run_test_driver
def run_test():
    p = GPS.Project("default")
    gps_assert(
        os.path.basename(os.path.normpath((p.exec_dir()))),
        "obj_%s"
        % (EXPECTED_TARGET_WINDOWS if os.name == "nt" else EXPECTED_TARGET_LINUX),
        "Target reference ignored in gpr1",
    )
