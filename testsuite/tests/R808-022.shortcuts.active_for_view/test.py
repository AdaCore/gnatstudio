"""
This test verifies the mechanism to enable shortcuts only in specific
views.
"""

from GPS import *
from gs_utils.internal.utils import *
import traceback


@run_test_driver
def run_test():
    # Assign the 'a' as a key shortcut to zoom in a browser
    GPS.Action("browser zoom in").key('a')

    # Open an editor and press on 'a': the editor should handle the key
    # press since the key shortcut for 'browser zoom in' should only be
    # active within browsers.

    buffer = GPS.EditorBuffer.get(GPS.File('a.adb'))
    send_key_event(ord('a'))
    yield wait_idle()

    gps_assert(buffer.get_chars(), "a",
               "The key press on 'a' is not handled by the editor.")
