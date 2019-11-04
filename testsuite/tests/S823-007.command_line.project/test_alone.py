"""
For user friendness, if the only file on the command line is a project
then load it => we are testing this case.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    gps_assert(GPS.Project.root().name(),
               "Foobar",
               "Failed to load the project on the command line")
    gps_assert(GPS.MDI.get("foobar.gpr"),
               None,
               "We should not opened the project file in the editor")
