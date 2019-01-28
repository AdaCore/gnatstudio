"""
This test checks that views deriving from Console_Process
(e.g: OS Shell) are restored when switching perspectives.
"""

from GPS import *
from gps_utils.internal.utils import *
import os

@run_test_driver
def run_test():
    if os.name == "nt" and os.getenv("COMSPEC"):
        console_name = os.getenv("COMSPEC")
    elif os.getenv("SHELL") and os.getenv("TERM"):        
        console_name = os.getenv("SHELL")

    # Spawn the console
    GPS.execute_action("open os shell")
    yield wait_for_mdi_child(console_name)

    # Switch from 'Default' to 'Debug' and go back to 'Default'
    GPS.MDI.load_perspective("Debug")
    GPS.MDI.load_perspective("Default")

    # Verify that the console has been restored
    gps_assert(GPS.MDI.get(console_name) is not None, True,
               "The OS Shell console should have been restored")
