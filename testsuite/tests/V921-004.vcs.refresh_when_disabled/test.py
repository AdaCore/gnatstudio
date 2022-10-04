"""
Test that VCS views are refreshed when disabling VCS for the
loaded project.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    history_view = History()
    yield history_view.open_and_yield()
    gps_assert(GPS.VCS2.active_vcs().name,
               "git",
               "Autodection failed for git repo")
    gps_assert(history_view.tree.is_visible(), True, "History view tree should be visible")
    prj = GPS.Project.load("no_vcs_prj/default.gpr")
    gps_assert(GPS.VCS2.active_vcs(),
               None,
               "There should be no VCS")
    gps_assert(history_view.tree.is_visible(), False, "History view should not be visible")