"""
This test verifies that we correctly display LSP formatting errors
in the Messages view, and in the Locations view when the format
is compatible.
"""
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("format file")
    yield wait_language_server("textDocument/formatting", "Ada")
    yield wait_idle()

    gps_assert(
        "main.adb:15:1: pretty printing already disabled at 4:1"
        in GPS.Console().get_text(),
        True,
        "The formattng error should be displayed in the Messages view",
    )

    msgs = GPS.Locations.list_locations("Formatting", "main.adb")
    gps_assert(str(msgs[0]), "main.adb:15:1", "Wrong location for formatting msg")
