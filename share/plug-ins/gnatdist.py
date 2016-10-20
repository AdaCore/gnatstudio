"""
This file provides support for using the gnatdist/po_gnatdist tool

gnatdist/po_gnatdist is a partitioning tool for distributed applications
which use features of the Distrbiuted System Annex.
This package provides syntax highlighting for partition configuration
language, and allows to run tool from GPS.
"""


import os_utils
import GPS

#  First, try to find gnatdist/po_gnatdist executable. po_gnatdist have
#  preference over gnatdist

gnatdist_tool = os_utils.locate_exec_on_path("po_gnatdist")
if gnatdist_tool == "":
    gnatdist_tool = os_utils.locate_exec_on_path("gnatdist")

#  If gnatdist/po_gnatdist tool was found, enable its support in GPS

if gnatdist_tool != "":
    GPS.parse_xml ("""
  <Language>
    <Name>gnatdist</Name>
    <Parent>Ada</Parent>
    <Spec_Suffix>.cfg</Spec_Suffix>
    <Keywords>^((c(onfiguration|hannel)|begin|use|i(s|n)|f(or|unction)|with|end|return|pr(ocedure|agma))\\b|partition(;|\s+))</Keywords>
    <Context>
      <New_Line_Comment_Start>--</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>False</Case_Sensitive>
    </Context>
    <Categories>
      <Category>
        <Name>partition</Name>
        <Pattern>\s+([-\w\d+_]+)(,\s+([-\w\d+_]+))*\s+:\s+Partition</Pattern>
        <Index>1</Index>
        <End_Index>1</End_Index>
      </Category>
      <Category>
        <Name>channel</Name>
        <Pattern>\s+([-\w\d+_]+)(,\s+([-\w\d+_]+))*\s+:\s+Channel</Pattern>
        <Index>1</Index>
        <End_Index>1</End_Index>
      </Category>
      <Category>
        <Name>subprogram</Name>
        <Pattern>^\s*(procedure|function)\s+([-\w\d+_:]+)((.*)|)(\s+(is|return)|\s*;)</Pattern>
        <Index>2</Index>
      </Category>
      <Category>
        <Name>configuration</Name>
        <Pattern>^configuration\s+([-\w\d+_]+)\s+is</Pattern>
        <Index>1</Index>
      </Category>
    </Categories>
  </Language>

  <project_attribute
    package="DSA"
    name="Configuration_File"
    editor_page="DSA"
    editor_section="DSA configuration"
    label="DSA configuration file name"
    hide_in="library_wizard"
    description="DSA configuration file to use for this project.">
    <string type="file"/>
  </project_attribute>

  <target-model name="gnatdist" category="">
    <description>PolyORB distributed application builder</description>
    <iconname>gps-custom-build-symbolic</iconname>
    <command-line>
      <arg>""" + gnatdist_tool + """</arg>
      <arg>-d</arg>
      <arg>-P%PP</arg>
      <arg>%attr(dsa'configuration_file)</arg>
    </command-line>
    <switches command="%(tool_name)s" lines="2" columns="1">
      <title line="1" column="1">Distribution Runtime Library (PCS)</title>
      <combo label="Distribution Runtime Library (PCS)" switch="--PCS=" noswitch="0" line="1" column="1">
        <combo-entry label="Default" value="0" />
        <combo-entry label="PolyORB" value="polyorb" />
        <combo-entry label="Garlic" value="garlic" />
      </combo>
      <title line="2" column="1">Gnadist switches</title>
      <check label="Progress bar" switch="-d" line="2" column="1"
             tip="Display a progress bar with information about how many files are left to be compiled"/>
      <check label="Consider all files, even readonly ali files" switch="-a" line="2" column="1"/>
      <check label="Force recompilations" switch="-f" line="2" column="1"/>
      <check label="Be quiet, do not display partitioning operations" switch="-q" line="2" column="1"/>
      <check label="Motivate all executed commands" switch="-v" line="2" column="1"/>
      <check label="Keep all temporary files" switch="-t" line="2" column="1"/>
    </switches>
  </target-model>

  <target model="gnatdist" category="_Project" name="Build _DSA Application...">
    <in-toolbar>TRUE</in-toolbar>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>""" + gnatdist_tool + """</arg>
      <arg>-d</arg>
      <arg>-P%PP</arg>
      <arg>%attr(dsa'configuration_file)</arg>
    </command-line>
  </target>""")
