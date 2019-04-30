"""
Test the behavior when loading an invalid project:
- The project file should be opened (even when Jump_To_First_Location = False)
- An empty project should be opened
- Reload of the empty project should try to reload the invalid project
"""

import GPS
from gps_utils.internal.utils import *

RELOAD = "reload project"


@run_test_driver
def test_driver():
    gps_assert(GPS.MDI.get("foo.gpr") is not None,
               True,
               "The invalid project file should be opened in the editor")

    gps_assert(GPS.Project.root().name(),
               "empty",
               "Empty project should be opened at startup")

    GPS.execute_action(RELOAD)
    gps_assert(GPS.Project.root().name(),
               "empty",
               "Foo is still invalid: Empty project should be opened")

    # Fix the project file
    buf = GPS.EditorBuffer.get(GPS.File("foo.gpr"))
    buf.insert(buf.at(1, 7), "dependencies/")
    buf.save()

    GPS.execute_action(RELOAD)
    gps_assert(GPS.Project.root().name(),
               "Foo",
               "The project is now valid and should be loaded")

    GPS.Project.load("dependencies/bar.gpr", force=True)
    gps_assert(GPS.Project.root().name(),
               "Bar",
               "Fails to load via python")
    GPS.execute_action(RELOAD)
    gps_assert(GPS.Project.root().name(),
               "Bar",
               "The last reload should have no effect: Bar is valid")
