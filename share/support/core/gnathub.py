"""
This file provides support for gnathub.
"""

import GPS
import gps_utils
import os_utils

gnathub_menu = "/Analyze/GNAThub/"
tools = {'codepeer':     'codepeer',
         'gcov':         'gcov',
         'gnatcoverage': 'gnatcov',
         'gnatcheck':    'gnatcheck',
         'gnatstack':    'gnatstack',
         'gnatmetric':   'gnatmetric',
         'spark2014':    'gnatprove'
         }


XML = r"""<?xml version="1.0" ?>
<GPS>
  <target-model name="gnathub">
    <iconname>gps-build-all-symbolic</iconname>
    <description>Run gnathub executable</description>
    <command-line>
      <arg>gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>%subdirsarg</arg>
    </command-line>
    <switches command="%(tool_name)s" columns="2">
    <title line="1" column="1">Available plugins</title>
    {}
    <title line="1" column="2">Other options</title>
    <field line="1" column="2" label="Execute" switch="--exec="
      tip="Python script to execute (implies --incremental)"/>
    <spin line="1" column="2" label="Parallel" switch="-j"
      tip="Number of jobs to run in parallel"
      max="99" min="0" default="0"/>
    <check line="1" column="2" label="Incremental" switch="-i"
      tip="Do not remove database if exists"/>
    <check line="1" column="2" label="Quiet" switch="-q"
      tip="Toggle quiet mode on"/>
    <check line="1" column="2" label="Verbose" switch="-v"
      tip="Toggle verbose mode on"/>
    </switches>
  </target-model>

  <target name="gnathub" category="_Project_" model="gnathub">
    <read-only>TRUE</read-only>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <in-menu>FALSE</in-menu>
    <command-line>
      <arg>gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>%subdirsarg</arg>
    </command-line>
  </target>

</GPS>
"""

# Template to insert into target-model for each gnathub plugin
template = r"""<check line="1" column="1"
 label="{}" switch="--plugins={}" tip="Run {} plugin" active="{}"/>
"""

# Check for gnathub executable and GNAThub module active status:

logger = GPS.Logger("GPS.INTERNAL.MODULE_GNAThub")

if os_utils.locate_exec_on_path("gnathub") and logger.active:
    checkboxes = ""
    for name, tool in tools.iteritems():
        if os_utils.locate_exec_on_path(tool):
            checkboxes += template.format(name, name, name, "on")
        else:
            checkboxes += template.format(name, name, name, "off")

    GPS.parse_xml(XML.format(checkboxes))

    @gps_utils.interactive(category="Gnathub",
                           menu=gnathub_menu+"Run...",
                           name="Run gnathub...")
    def show_dialog_and_run_gnathub():
        target = GPS.BuildTarget("gnathub")
        target.execute(synchronous=False)

    @gps_utils.hook("compilation_finished")
    def __hook(category, target_name="", mode_name="", status=""):
        if not status and target_name == "gnathub":
            GPS.execute_action("gnathub display analysis")
