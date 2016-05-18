"""
This file provides support for gnathub.
"""

import GPS
import gps_utils
import os_utils

gnathub_menu = "/Gnathub/Run "
tools = ['codepeer', 'gcov', 'gnatcoverage', 'gnatcheck', 'gnatmetric',
         'gnatprove']


def register_menu(tool):
    @gps_utils.interactive(category="Gnathub",
                           menu=gnathub_menu+tool,
                           name="Run gnathub: " + tool)
    def action():
        target = GPS.BuildTarget("gnathub")
        target.execute(extra_args="--incremental --plugins=" + tool)

XML = r"""<?xml version="1.0" ?>
<GPS>
  <target-model name="gnathub">
    <iconname>call-start</iconname>
    <description>Run gnathup executable</description>
    <command-line>
      <arg>gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
    </command-line>
    <switches command="%(tool_name)s" columns="1">
    <field label="Execute" switch="--exec="
      tip="Python script to execute (implies --incremental)"/>
    <field label="Plugins" switch="--plugins="
      tip="Comma separated list of plugins to execute"/>
    <spin label="Parallel" switch="-j"
      tip="Number of jobs to run in parallel"
      max="99" min="0" default="0"/>
    <check label="Incremental" switch="-i"
      tip="Do not remove database if exists"/>
    <check label="Quiet" switch="-q"
      tip="Toggle quiet mode on"/>
    <check label="Verbose" switch="-v"
      tip="Toggle verbose mode on"/>
    </switches>
  </target-model>

  <target name="gnathub" category="_Project_" model="gnathub">
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <in-menu>FALSE</in-menu>
    <command-line>
      <arg>gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
    </command-line>
  </target>

</GPS>
"""


def on_compilation_finished(hook, category,
                            target_name="", mode_name="", status=""):

    if not status and target_name in ["gnathub"]:
        GPS.execute_action("gnathub display analysis")

# Check for gnathub executable:

if os_utils.locate_exec_on_path("gnathub"):
    GPS.parse_xml(XML)
    GPS.Hook('compilation_finished').add(on_compilation_finished)

    for J in tools:
        register_menu(J)
