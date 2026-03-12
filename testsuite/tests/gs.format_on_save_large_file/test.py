"""
Test that format-on-save works correctly with large files where LSP
formatting is asynchronous.

This specifically tests the race condition where the file could be
saved to disk before the async LSP formatting response arrives.
After saving, we verify that:
  - the buffer contents are properly formatted,
  - the editor is marked as saved (not modified),
  - the file on disk matches the buffer contents exactly.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    # Open the large Ada file and wait for the LSP to be ready
    buf = GPS.EditorBuffer.get(GPS.File("gps-kernel-mdi.adb"))
    yield wait_tasks()

    # Record the properly formatted content
    expected = buf.get_chars()

    # Mess up the indentation: add leading spaces to several lines
    # spread across the file to make the formatting non-trivial
    for line in [20, 50, 100, 200, 300, 500]:
        buf.insert(buf.at(line, 1), "    ")
    yield timeout(3000)

    gps_assert(
        buf.is_modified(), True, "Buffer should be modified after inserting spaces"
    )

    # Save the file: this triggers the autoformat plugin which calls
    # buf.indent() in the before_file_saved hook. With LSP formatting,
    # the formatting is asynchronous and the save must be deferred
    # until the formatting response arrives.
    buf.save()
    yield timeout(3000)

    # Check that the buffer is properly formatted
    gps_assert(buf.get_chars(), expected, "Buffer is not properly formatted after save")

    # Check that the editor is marked as saved
    gps_assert(
        buf.is_modified(), False, "Buffer is still marked as modified after save"
    )

    # Check that the file on disk matches the formatted buffer contents
    with open(buf.file().path, "r") as f:
        on_disk = f.read()
    gps_assert(
        on_disk, expected, "File on disk does not match formatted buffer contents"
    )
