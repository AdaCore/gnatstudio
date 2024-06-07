"""
Check that no EditorBuffer are leaking when loading another project.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("LSP-Ada-Diagnostics").set(True)
    GPS.execute_action("Restart ada language server")
    yield hook("language_server_started")

    # Open some buffers
    GPS.EditorBuffer.get(GPS.File("foo.adb"))
    GPS.EditorBuffer.get(GPS.File("bar.ads"))
    # Open and close a buffer
    GPS.EditorBuffer.get(GPS.File("bar.adb")).close()

    # Load another project
    GPS.Project.load("test2.gpr")
    # Wait a bit for the diagnostics to be cleaned after the DidClose requests
    yield timeout(500)
    GPS.EditorBuffer.get(GPS.File("foobar.adb"))

    # All the previous buffers should be cleaned
    gps_assert(
        GPS.EditorBuffer.get(GPS.File("foo.adb"), open=False),
        None,
        "foo.adb should be closed",
    )
    gps_assert(
        GPS.EditorBuffer.get(GPS.File("bar.ads"), open=False),
        None,
        "bar.ads should be closed",
    )
    gps_assert(
        GPS.EditorBuffer.get(GPS.File("bar.adb"), open=False),
        None,
        "bar.adb should be closed",
    )
