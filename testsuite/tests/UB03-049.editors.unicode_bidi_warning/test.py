"""
This test checks that GNAT Studio displays a warning when an editor
contains unicode bidirectional characters.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Open a file that contains a Right To Left Override unicode
    # character.
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    # Set the charset to UTF8
    prop = Editor_Properties_Dialog()
    yield prop.open_and_yield("")
    charset = prop.get_character_set_entry()
    while charset.get_active_text() != "Unicode UTF-8":
        charset.set_active(charset.get_active() + 1)
    yield prop.ok()

    gps_assert(
        "Warning: the file contains bidirectional" in GPS.Console().get_text(),
        True,
        "A warning for unicode bidirectional characters should be displayed",
    )
