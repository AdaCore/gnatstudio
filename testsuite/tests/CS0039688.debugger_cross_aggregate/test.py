"""Test selection of the debugger executabe for aggregeted cross project"""

import GS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    console = GS.Console("Messages")

    console.clear()
    GPS.execute_action("/Debug/Initialize/main/main")
    text = console.get_text()
    found = (
        text.find("Could not locate executable on path: i586-elf-lynxos178e-gdb") != -1
    )

    gps_assert(found, True, "Incorrect debugger's executable used")
