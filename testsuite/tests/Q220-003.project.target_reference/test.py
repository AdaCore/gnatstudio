"""
This test verifies that target reference is taken
into account for project directories
"""
import GPS
import os.path
import platform
from gs_utils.internal.utils import run_test_driver, gps_assert

EXPECTED_TARGET_NATIVE_LINUX = "x86_64-pc-linux-gnu"
EXPECTED_TARGET_NATIVE_WINDOWS = "x86_64-w64-mingw32"
EXPECTED_TARGET_AARCH64_LINUX = "aarch64-linux-gnu"


@run_test_driver
def run_test():
    p = GPS.Project("default")
    gps_assert(
        os.path.basename(os.path.normpath((p.exec_dir()))),
        "obj_%s"
        % (
            EXPECTED_TARGET_NATIVE_WINDOWS
            if os.name == "nt"
            else (
                EXPECTED_TARGET_NATIVE_LINUX
                if platform.machine() == "x86_64"
                else EXPECTED_TARGET_AARCH64_LINUX
            )
        ),
        "Target reference ignored in gpr1 for platform: %s" % platform.machine(),
    )
