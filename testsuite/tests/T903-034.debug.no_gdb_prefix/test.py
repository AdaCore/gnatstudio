"""
This test verifies that we don't prefix GDB by 'x86-linux' or
'x86-windows' when the project targets a 32-bit host platform.
"""

import GPS
from gs_utils.internal.utils import *
import platform


@run_test_driver
def run_test():
    prj = GPS.Project.root()

    if platform.system() == "Windows":
        prj.set_scenario_variable("HOST", "Windows")
    else:
        prj.set_scenario_variable("HOST", "Linux")

    prj.recompute()
    yield wait_tasks()

    GPS.execute_action("/Debug/Initialize/no main file")
    yield wait_idle()

    debug = GPS.Debugger.get()
    gps_assert(debug != None, True, "The debugger failed to spawn.")
