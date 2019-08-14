"""
This file provides support for gnatstack (static stack usage).
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import gps_utils

TARGET_NAME = "Run GNATStack"

XML_BASE = ("""
  <!--  Support for running GNATStack as a build target  -->

  <target-model name="gnathub_gnatstack" category="">
    <description>Run GNATStack for analysis</description>
    <command-line>
      <arg>%gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>%subdirsarg</arg>
      <arg>--plugins=gnatstack</arg>
    </command-line>
    <switches columns="1" lines="1">
      <check label="Incremental mode" switch="-i" column="1"
            tip="Append this run results to the previous runs."/>
    </switches>
  </target-model>

  <target model="gnathub_gnatstack" category="GNATStack" name="{target_name}">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <command-line>
      <arg>%gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>%subdirsarg</arg>
      <arg>--plugins=gnatstack</arg>
    </command-line>
  </target>

  <!--  Support for GNATStack's switches in Project Properties Editor  -->

  <tool name="GNATstack" package="Stack" attribute="switches" index="">
    <language>Ada</language>
    <language>C</language>
    <language>C++</language>
    <switches>
      <title line="1" column="1">Entry points</title>
      <field line="1" column="1" switch="-e" separator=" "
       label="use specific list of subprograms as entry points (e1,e2,...)"/>
      <field line="1" column="1" switch="-r" separator=" "
             label="use all subprograms matching reg-exp as entry points"/>
      <check line="1" column="1" switch="-a"
             label="use all subprograms as entry points"/>

      <title line="2" column="1">Stack frame sizes</title>
      <spin line="2" column="1" switch="-c" separator=" "
            min="0" max="2147483647" default="0"
            label="size of frame for cycle entry"/>
      <spin line="2" column="1" switch="-d" separator=" "
            min="0" max="2147483647" default="0"
            label="size of frame for unbounded (dynamic) calls"/>
      <spin line="2" column="1" switch="-u" separator=" "
            min="0" max="2147483647" default="0"
            label="size of frame for unknown (external) calls"/>

      <title line="3" column="1">CI in object files</title>
      <field line="3" column="1" switch="-s" separator="="
             label="name of the section where the .ci is in the object"/>
      <field line="3" column="1" switch="-oc" separator="="
             label="name of the objcopy executable"/>

      <check line="4" column="1" switch="-ca" label="extract all cycles"/>
      <check line="4" column="1" switch="-k" label="keep temporary files"/>
    </switches>
  </tool>
""").format(target_name=TARGET_NAME)


@gps_utils.interactive(
    category="GNATHUB_GNATSTACK", name="analyze stack usage")
def analyze_stack():
    target = GPS.BuildTarget(TARGET_NAME)
    target.execute(synchronous=False)


@gps_utils.hook("compilation_finished")
def __hook(category, target_name="", mode_name="", status="", *arg):
    if not status and target_name == TARGET_NAME:
        GPS.execute_action("gnathub display analysis")


GPS.parse_xml(XML_BASE)
