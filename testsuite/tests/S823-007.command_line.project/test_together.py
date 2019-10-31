"""
If we have multiple file on the command line and no project file via -P.
=> Open the file in the editors
"""
import GPS
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    gps_assert(GPS.Project.root().name(),
               "Default",
               "The default project should be loaded")
    gps_assert(GPS.MDI.get("foobar.gpr") is not None,
               True,
               "The file in the command line should be opened in the editors")
