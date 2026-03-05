"""
Test that ClearCase does not query the status for files outside the
working directory. This prevents long loading times when the Files view
shows system directories like /, /bin/, /etc/, etc.
"""

import GPS
import os
from pathlib import Path
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    # Verify ClearCase was autodetected
    vcs = GPS.VCS2.active_vcs()
    gps_assert(vcs.name, "clearcase native", "Autodetection failed")

    # The log file is in the test directory (where cleartool script lives)
    test_dir = Path(GPS.Project.root().file().directory())
    log_file = test_dir / "cleartool_calls.log"

    # The working directory is my_tag (as returned by discover_working_dir)
    working_dir = Path(vcs.working_dir.path).resolve()

    # Open a test file that is inside the ClearCase view (should be queried)
    inside_path = working_dir / "inside_file.ads"
    inside_file = GPS.File(str(inside_path))

    # Files outside the ClearCase view (should be filtered out)
    outside_files = [
        GPS.File("/etc/hosts"),
        GPS.File("/bin/ls"),
    ]

    # Request status for a mix of inside and outside files
    all_files = [inside_file] + outside_files

    # Call the method that should filter files
    # Use ensure_status_for_files which eventually calls async_fetch_status_for_files
    vcs.ensure_status_for_files(all_files)

    # Wait for background processing
    yield wait_tasks(other_than=known_tasks)

    # Check the log file to verify what was actually passed to cleartool
    if log_file.exists():
        log_content = log_file.read_text()
        GPS.Console().write("Cleartool log:\n" + log_content + "\n")

        # Verify that the inside file path appears in the log
        gps_assert(
            str(inside_file.path) in log_content or "inside_file.ads" in log_content,
            True,
            "File inside working dir should be queried",
        )

        # Verify that outside files do NOT appear in the log
        for outside in ["/etc/hosts", "/bin/ls"]:
            gps_assert(
                outside not in log_content,
                True,
                f"File outside working dir should NOT be queried: {outside}",
            )
    else:
        # If no log file, the filter may have excluded all "outside" files
        # but we still need to verify the inside file was processed
        gps_fatal_error("Cleartool log file not found.")
