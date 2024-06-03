"""
Test setting of label of the task when builder reports phase info
"""

from gs_utils.internal.utils import (gps_assert, run_test_driver,
                                     wait_idle, wait_tasks, timeout)
#import GPS

XML = r"""<?xml version="1.0" ?>
<GNAT_Studio>
<target model="builder" category="_Project" name="Run Test Build Simulation">
    <in-toolbar>FALSE</in-toolbar>
    <iconname>gps-build-all-symbolic</iconname>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
       <arg>sh</arg>
       <arg>simulate_build.sh</arg>
    </command-line>
</target>
</GNAT_Studio>
"""

GPS.parse_xml(XML)

@run_test_driver
def driver():
    yield wait_tasks()
    GPS.execute_action("/Build/Project/Run Test Build Simulation")

    stage_1 = False
    stage_2 = False

    while len(GPS.Task.list()) != 0:
        if GPS.Task.list()[0].label() == "Stage 1":
            stage_1 = True

        if GPS.Task.list()[0].label() == "Stage 2":
            stage_2 = True

        yield timeout(100)

    gps_assert(True, stage_1, "Stage 1 was not displayed")
    gps_assert(True, stage_2, "Stage 2 was not displayed")
