"""
Check the default project when GNATSTUDIO_CUSTOM_PATH is defined as a list of
directories.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    proj = GPS.Project.root()
    gps_assert(proj.file().base_name(), "default.gpr", "Wrong project name")

    gps_assert(
        proj.get_attribute_as_list("Switches", "Compiler", "ada"),
        ["-O2", "-gnatWa"],
        "Wrong switches for custom default",
    )
