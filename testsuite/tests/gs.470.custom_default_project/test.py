"""
Check the default project when defined with GNATSTUDIO_CUSTOM_PATH as a file
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    proj = GPS.Project.root()
    gps_assert(proj.file().base_name(), "default.gpr", "Wrong project name")

    gps_assert(
        proj.get_attribute_as_list("Switches", "Compiler", "ada"),
        ["-g", "-Og", "-gnatwa", "-gnatyy"],
        "Wrong switches for custom default",
    )
