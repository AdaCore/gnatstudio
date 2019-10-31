"""
This test verifies that target reference is taken
into account for project directories
"""
import GPS
import os.path
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    p = GPS.Project("default")
    gps_assert(os.path.basename(os.path.normpath((p.exec_dir()))),
               "obj_",
               "Target reference ignored in gpr1")
    #Deactivate this until we have moved to GPR2
    #Once moved to gpr2 this test should start failing
    #and be modified accordingly.
