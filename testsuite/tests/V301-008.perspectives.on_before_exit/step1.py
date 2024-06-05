"""
Step 1: open a file and block the exit
"""
import GPS
from gs_utils.internal.utils import *


def before_exit(hook):
    # Block basic quit
    return False


@run_test_driver
def test_driver():
    GPS.Hook("before_exit_action_hook").add(before_exit)
    # Open a file: it must update perspectives6.xml
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.exit()
