#!/usr/bin/python
# -*- coding: utf-8 -*-

############################################################################
# No user customization below this line
############################################################################

"""
This file provides support for using the SPARK 2014 toolset.
"""

import GPS
import os_utils
import os.path
import tool_output
import json
import re

# We create the actions and menus in XML instead of python to share the same
# source for GPS and GNATbench (which only understands the XML input for now).

xml_gnatprove_menus = """<?xml version="1.0"?>
  <GNATPROVE>
    <filter name="Inside Subprogram Context" language="Ada" shell_lang="python"
        shell_cmd="spark2014.inside_subp_context(GPS.current_context())" />

    <action name="Examine All Action" category="GNATprove" output="none">
       <shell
         lang="python">spark2014.on_examine_all(GPS.current_context())</shell>
    </action>
    <action
        name="Examine Root Project Action" category="GNATprove" output="none">
       <shell
         lang="python">spark2014.on_examine_root_project(GPS.current_context())
       </shell>
    </action>
    <action name="Examine File Action" category="GNATprove" output="none">
       <filter_and>
          <filter language="Ada" />
          <filter id="Source editor" />
       </filter_and>
       <shell
         lang="python">spark2014.on_examine_file(GPS.current_context())
       </shell>
    </action>
    <action
      name="Examine Subprogram Action" category="GNATprove" output="none">
       <filter id="Inside Subprogram Context" />
       <shell
         lang="python">spark2014.on_examine_subp(GPS.current_context())
       </shell>
    </action>
    <action name="Prove All Action" category="GNATprove" output="none">
       <shell
         lang="python">spark2014.on_prove_all(GPS.current_context())</shell>
    </action>
    <action
      name="Prove Root Project Action" category="GNATprove" output="none">
       <shell
         lang="python">spark2014.on_prove_root_project(GPS.current_context())
       </shell>
    </action>
    <action
      name="Prove File Action" category="GNATprove" output="none">
       <filter_and>
          <filter language="Ada" />
          <filter id="Source editor" />
       </filter_and>
       <shell
         lang="python">spark2014.on_prove_file(GPS.current_context())</shell>
    </action>
    <action name="Prove Subprogram Action" category="GNATprove" output="none">
       <filter id="Inside Subprogram Context" />
       <shell
         lang="python">spark2014.on_prove_subp(GPS.current_context())</shell>
    </action>
    <action name="Prove Line Action" category="GNATprove" output="none">
       <filter language="Ada" shell_lang="python"
         shell_cmd="spark2014.is_file_context(GPS.contextual_context())" />
       <shell
         lang="python">spark2014.on_prove_line(GPS.contextual_context())
       </shell>
    </action>
    <action name="Prove Check Action" category="GNATprove" output="none">
       <filter language="Ada" shell_lang="python"
        shell_cmd="spark2014.prove_check_context(GPS.contextual_context())" />
       <shell
         lang="python">spark2014.on_prove_check(GPS.contextual_context())
       </shell>
    </action>
    <action name="Show Report Action" category="GNATprove" output="none">
        <shell
          lang="python">spark2014.on_show_report(GPS.current_context())
        </shell>
    </action>
    <action name="Clean Proofs Action" category="GNATprove" output="none">
        <shell
          lang="python">spark2014.on_clean_up(GPS.current_context())</shell>
    </action>

    <submenu before="Window">
      <Title>_%(prefix)s</Title>
        <menu action="Examine All Action">
          <Title>Examine All</Title>
        </menu>
        <menu action="Examine Root Project Action">
          <Title>Examine Root Project</Title>
        </menu>
        <menu action="Examine File Action">
          <Title>Examine File</Title>
        </menu>
        <menu><title/></menu>
        <menu action="Prove All Action">
          <Title>Prove All</Title>
        </menu>
        <menu action="Prove Root Project Action">
          <Title>Prove Root Project</Title>
        </menu>
        <menu action="Prove File Action">
          <Title>Prove File</Title>
        </menu>
        <menu><title/></menu>
        <menu action="Show Report Action">
          <Title>Show Report</Title>
        </menu>
        <menu action="Clean Proofs Action">
          <Title>Clean Proofs</Title>
        </menu>
        <menu action="Remove Editor Highlighting Action">
          <Title>Remove Editor Highlighting</Title>
        </menu>
    </submenu>

    <contextual action="Examine File Action">
      <Title>%(prefix)s/Examine File</Title>
    </contextual>
    <contextual action="Examine Subprogram Action">
      <Title>%(prefix)s/Examine Subprogram</Title>
    </contextual>
    <contextual action="Prove File Action">
      <Title>%(prefix)s/Prove File</Title>
    </contextual>
    <contextual action="Prove Subprogram Action">
      <Title>%(prefix)s/Prove Subprogram</Title>
    </contextual>
    <contextual action="Prove Line Action">
      <Title>%(prefix)s/Prove Line</Title>
    </contextual>
    <contextual action="Prove Check Action">
      <Title>%(prefix)s/Prove Check</Title>
    </contextual>

    <doc_path>share/doc/spark</doc_path>

    <documentation_file>
      <name>html/ug/index.html</name>
      <descr>SPARK 2014 Toolset User's Guide</descr>
      <category>%(prefix)s</category>
      <menu before="About">/Help/%(prefix)s/SPARK 2014 Toolset User's Guide
      </menu>
    </documentation_file>

    <documentation_file>
      <name>html/lrm/index.html</name>
      <descr>SPARK 2014 Reference Manual</descr>
      <category>%(prefix)s</category>
      <menu before="About">/Help/%(prefix)s/SPARK 2014 Reference Manual</menu>
    </documentation_file>

    <action name="spark2014_example_binary_search" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/binary_search/test.gpr"</shell>
      <shell>Editor.edit "binary_search.adb"</shell>
      <shell>Editor.edit "binary_search.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_binary_search">
        <title>Binary Search</title>
      </menu>
    </submenu>

    <action name="spark2014_example_binary_search_unconstrained" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/binary_search_unconstrained/test.gpr"
      </shell>
      <shell>Editor.edit "binary_search.adb"</shell>
      <shell>Editor.edit "binary_search.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_binary_search_unconstrained">
        <title>Binary Search Unconstrained</title>
      </menu>
    </submenu>

    <action name="spark2014_example_contract_cases" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/contract_cases/test.gpr"</shell>
      <shell>Editor.edit "p.adb"</shell>
      <shell>Editor.edit "p.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_contract_cases">
        <title>Contract Cases</title>
      </menu>
    </submenu>

    <action name="spark2014_example_database" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/database/test.gpr"</shell>
      <shell>Editor.edit "database.adb"</shell>
      <shell>Editor.edit "database.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_database">
        <title>Database</title>
      </menu>
    </submenu>

    <action name="spark2014_example_formal_queue" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/formal_queue/test.gpr"</shell>
      <shell>Editor.edit "queue.adb"</shell>
      <shell>Editor.edit "queue.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_formal_queue">
        <title>Formal Queue</title>
      </menu>
    </submenu>

    <action name="spark2014_example_intro" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/intro/test.gpr"</shell>
      <shell>Editor.edit "pricing.adb"</shell>
      <shell>Editor.edit "pricing.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_intro">
        <title>Intro</title>
      </menu>
    </submenu>

    <action name="spark2014_example_longest_common_prefix" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/longest_common_prefix/test.gpr"</shell>
      <shell>Editor.edit "lcp.adb"</shell>
      <shell>Editor.edit "lcp.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_longest_common_prefix">
        <title>Longest Common Prefix</title>
      </menu>
    </submenu>

    <action name="spark2014_example_max_and_sum" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/max_and_sum/test.gpr"</shell>
      <shell>Editor.edit "maxandsum.adb"</shell>
      <shell>Editor.edit "maxandsum.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_max_and_sum">
        <title>Max and Sum</title>
      </menu>
    </submenu>

    <action name="spark2014_example_natural_set" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/natural/test.gpr"</shell>
      <shell>Editor.edit "natural_set.adb"</shell>
      <shell>Editor.edit "natural_set.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_natural_set">
        <title>Natural Set</title>
      </menu>
    </submenu>

    <action name="spark2014_example_ring_buffer" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/ring_buffer/test.gpr"</shell>
      <shell>Editor.edit "ring_buf.adb"</shell>
      <shell>Editor.edit "ring_buf.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_ring_buffer">
        <title>Ring Buffer</title>
      </menu>
    </submenu>

    <action name="spark2014_example_segway" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/segway/test.gpr"</shell>
      <shell>Editor.edit "segway.adb"</shell>
      <shell>Editor.edit "segway.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_segway">
        <title>Segway</title>
      </menu>
    </submenu>

    <action name="spark2014_example_spark_io" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/spark_io/test.gpr"</shell>
      <shell>Editor.edit "spark-text_io.ads"</shell>
      <shell>Editor.edit "hello_world.adb"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_spark_io">
        <title>SPARK IO</title>
      </menu>
    </submenu>

  </GNATPROVE>
"""

xml_gnatprove = """<?xml version="1.0"?>
  <GNATPROVE>
    <tool name="GNATprove" package="Prove" attribute="switches" index="">
      <language>Ada</language>
      <switches switch_char="-">
        <title line="1">Proof</title>
         <combo label="Main mode" switch="--mode" noswitch="all"
               separator="=" column="1"
               tip="Main mode of formal verification" >
            <combo-entry
              label="check"
              value="check"
              tip="Check SPARK restrictions for code where SPARK_Mode=On"/>
            <combo-entry
              label="flow"
              value="flow"
              tip="Prove object initialization, globals and depends contracts"
            />
            <combo-entry
              label="prove"
              value="prove"
              tip="Prove subprogram contracts and absence of run-time errors"
            />
            <combo-entry label="all" value="all"
                         tip="Activates all modes"/>
         </combo>
        <combo line="1" label="Report mode" switch="--report" separator="="
               noswitch="fail" tip="Amount of information reported">
          <combo-entry label="fail" value="fail"
                       tip="Only failed proof attempts"/>
          <combo-entry label="all" value="all"
                       tip="All proof attempts"/>
          <combo-entry label="statistics" value="statistics"
                       tip="Detailed proof attempts"/>
        </combo>
<combo
label="Proof strategy"
switch="--proof"
noswitch="per_check"
separator="=" column="2"
tip="Formulas generated for each check (faster) or each path (more precise)" >
    <combo-entry label="One proof per check" value="per_check"
                 tip="Generate one formula per check"/>
    <combo-entry label="One proof per path" value="per_path"
                 tip="Generate one formula per path for each check"/>
    <combo-entry
    label="Progressively split"
    value="progressive"
    tip="Start ith one formula per check, then split into paths when needed"/>
</combo>
        <spin label="Prover timeout" switch="--timeout="
              default="1" min="1" max="3600"
              tip="Set the prover timeout (in s) for individual proofs" />
        <spin
          label="Prover max steps"
          switch="--steps="
          default="0"
          min="0"
          max="1000000"
          tip="Set the prover maximum number of steps for individual proofs"/>
        <title line="1" column="2">Process control</title>
        <spin label="Multiprocessing" column="2" switch="-j"
              default="1" min="1" max="100"
              tip="Use N processes to compile and prove.
                   On a multiprocessor machine compilation and proof
                   will occur in parallel" />
      </switches>
    </tool>

    <target-model name="gnatprove-examine">
       <description>Target model for GNATprove Examine commands</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=flow</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <persistent-history>False</persistent-history>
    </target-model>

    <target-model name="gnatprove-prove">
       <description>Target model for GNATprove Prove commands</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <switches command="%(tool_name)s">
         <title column="1" line="1" >General</title>
         <spin label="Multiprocessing" switch="-j" min="0" max="1000"
          default="1" separator="" column="1"
          tip="Use N processes to carry out the processing
               (0 means use as many cores as available on the machine)." />
         <combo
           label="Warnings"
           switch="--warnings" noswitch="continue"
           separator="=" column="1"
           tip="Stop analysis after warnings or continue,
                or do not issue warnings">
             <combo-entry label="stop after warnings" value="error"
              tip="Warnings are considered as errors and stop the analysis"/>
             <combo-entry label="continue when warnings" value="continue"
              tip="issue warnings, but continue analysis"/>
             <combo-entry label="do not issue warnings" value="off"
             tip="Do not issue warnings at all"/>
         </combo>
         <check
           label="Force re-analysis" switch="-f" column="1"
           tip="Re-start analysis from scratch, ignoring previous results" />
         <check label="Report checks proved" switch="--report=all" column="1"
                tip="Report the status of all checks, including those proved"
         />
         <title column="2" line="1" >Prover</title>
         <combo
           label="Proof strategy"
           switch="--proof" noswitch="per_check"
           separator="=" column="2"
           tip="Formulas generated for each check (faster)
                or each path (more precise)">
             <combo-entry label="One proof per check" value="per_check"
                          tip="Generate one formula per check"/>
             <combo-entry label="One proof per path" value="per_path"
                          tip="Generate one formula per path for each check"/>
             <combo-entry
             label="Progressively split"
             value="progressive"
             tip="Start with one formula per check,
                  then split into paths when needed"/>
         </combo>
         <spin label="Prover timeout" switch="--timeout=" column="2"
                default="1" min="1" max="3600"
                tip="Set the prover timeout (in s) for individual proofs" />
         <field label="Alternate prover" switch="--prover=" column="2"
                tip="Alternate prover to use instead of Alt-Ergo" />
       </switches>
       <persistent-history>False</persistent-history>
    </target-model>

    <target model="gnatprove-examine" name="Examine All" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=flow</arg>
          <arg>--ide-progress-bar</arg>
          <arg>-U</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-examine" name="Examine Root Project"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=flow</arg>
          <arg>--ide-progress-bar</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-examine"
            name="Examine Single File" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=flow</arg>
          <arg>--ide-progress-bar</arg>
          <arg>-u</arg>
          <arg>%fp</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-examine" name="Examine Subprogram"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=flow</arg>
          <arg>--ide-progress-bar</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove All" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>-U</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove Root Project"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove File" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>-u</arg>
          <arg>%fp</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove Subprogram"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove Line" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>--limit-line=%f:%l</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove Line Location"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>

    <target-model name="gnatprove_clean">
       <description>Target model for GNATprove for cleaning</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <persistent-history>False</persistent-history>
    </target-model>

    <target model="gnatprove_clean" name="Clean Proofs" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--clean</arg>
       </command-line>
    </target>

    <target model="gnatprove-prove" name="Prove Check" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         location_parser
         end_of_build
       </output-parsers>
    </target>
  </GNATPROVE>
"""

# constants that are required by the plug-in

toolname = 'gnatprove'
obj_subdir_name = toolname
report_file_name = toolname + '.out'
prefix = 'SPARK'
menu_prefix = '/' + prefix
examine_all = 'Examine All'
examine_root_project = 'Examine Root Project'
examine_file = 'Examine Single File'
examine_subp = 'Examine Subprogram'
prove_all = 'Prove All'
prove_root_project = 'Prove Root Project'
prove_file = 'Prove File'
prove_subp = 'Prove Subprogram'
prove_line = 'Prove Line'
# used to launch Prove Line from Location View
prove_line_loc = 'Prove Line Location'
# in case of manual provers, prove_check is
# the only one allowed to open editors
prove_check = 'Prove Check'
show_report = 'Show Report'
clean_up = 'Clean Proofs'

Default_Trace_Color = "#00ffff"
Overlay_Name = "Gnatprove_Trace_Overlay"

Color_Pref_Name = 'Plugins/gnatprove/color_trace'

GPS.Preference(Color_Pref_Name).create(
    'Highlight color for trace',
    'color',
    'color used to highlight trace lines.' +
    ' You must restart GPS to take changes into account.',
    Default_Trace_Color)


def get_trace_overlay(buf):
    """retrieve the trace overlay for a buffer. If the buffer hasn't got one
       yet, create it, add it to the buffer, and return it.
    """

    if not hasattr(buf, Overlay_Name):
        o = buf.create_overlay('trace overlay')
        o.set_property('paragraph-background',
                       GPS.Preference(Color_Pref_Name).get())
        setattr(buf, Overlay_Name, o)
    return getattr(buf, Overlay_Name)


# helper functions that do not really fit elsewhere
def goto_location(sloc):
    """go to the location defined by the given GPS.FileLocation"""

    buf = GPS.EditorBuffer.get(sloc.file())
    v = buf.current_view()
    GPS.MDI.get_by_child(v).raise_window()
    v.goto(buf.at(sloc.line(), sloc.column()))
    v.center()


# This is the on_exit callback for the editor process
def check_proof_after_close(proc, ex_st, outp):
    """run gnatprove to check a proof after the external editor has been
       closed
    """
    if not proc._is_killed:
        try:
            vc_kind = get_vc_kind(proc._proc_msg)
            llarg = limit_line_option(proc._proc_msg, vc_kind)
            GPS.BuildTarget(prove_check).execute(extra_args=[llarg],
                                                 synchronous=False)
        except TypeError:
            pass


# For some reason, when the process is killed,
# on_exit function of the process is called twice
# and asking twice to relaunch GNATprove.
# So instead of that if the process was killed we don't do anything
def editor_before_kill(proc, outp):
    """check if editor was killed"""
    proc._is_killed = True


trace_msg = None
trace_lines = []


def show_trace(lines):
    """show the trace given by the lines"""
    f = None
    for sloc in lines or []:
        if sloc.file() != f:
            f = sloc.file()
            buf = GPS.EditorBuffer.get(f)
            goto_location(sloc)
            overlay = get_trace_overlay(buf)
        buf.apply_overlay(overlay,
                          buf.at(sloc.line(), 1),
                          buf.at(sloc.line(), 1))


def remove_trace(lines):
    """remove the trace given by the lines"""
    f = None
    for sloc in lines or []:
        if sloc.file() != f:
            f = sloc.file()
            buf = GPS.EditorBuffer.get(f, open=False)
            if buf:
                overlay = get_trace_overlay(buf)
                buf.remove_overlay(overlay)


def toggle_trace(msg, lines):
    """toggle the trace for the given msg and lines"""
    global trace_msg, trace_lines
    if trace_msg is None:
        trace_msg = msg
        trace_lines = lines
        show_trace(lines)
    elif trace_msg == msg:
        remove_trace(trace_lines)
        trace_msg = None
    else:
        remove_trace(trace_lines)
        trace_msg = msg
        trace_lines = lines
        show_trace(lines)


class GNATprove_Parser(tool_output.OutputParser):

    """Class that parses messages of the gnatprove tool, and creates
       decorates the messages coming from GNATprove with actions (showing
       traces) when needed.
       In IDE mode, and when extra info is available for a message, Gnatprove
       appends a symbol [#id] to a message, where "id" is a number  which is
       unique for this unit and this message.
       The GNATprove parser strips the extra symbol from the message so that
       it's not visible in GPS, and builds up a mapping
         msg -> id
       Once GNATprove is terminated, for each msg which has an entry in this
       mapping, the parser opens the files "unit.flow" and "unit.proof", which
       are JSON files. See the [parsejson] function for the format of these
       files.
       Once these files are parsed, the GNATprove parser now knows the extra
       information associated to a message, if any. See [act_on_extra_info] to
       know what is down with this extra information.
    """

    def __init__(self, child):
        tool_output.OutputParser.__init__(self, child)
        # holds the unit names for which extra info is retrieved
        self.units_with_extra_info = []
        # holds the mapping "msg" -> msg_id
        self.msg_id = {}
        self.regex = re.compile(r"(.*)\[#([0-9]+)\]$")
        # holds the mapping "unit,msg_id" -> extra_info
        self.extra_info = {}

    def build_msg_full_text(self, file, line, col, text):
        """Given a msg text and location, return the string
           "file:line:col:msg"
        """
        return file + ':' + str(line) + ':' + str(col) + ': ' + text

    def pass_output(self, text, command):
        """pass the text on to the next output parser"""
        if self.child:
            self.child.on_stdout(text + '\n', command)

    def error_msg_from_json(self, msg):
        """Given a JSON dict that contains the data for a message, print a
        corresponding "compiler-like" message on the GPS Console"""

        text = self.build_msg_full_text(msg['file'],
                                        msg['line'],
                                        msg['col'],
                                        msg['message'])
        return text

    def parse_trace_file(self, filename):
        """ parse the trace file as a list of "file:line" information and
            return the result
        """

        lines = []
        if os.path.isfile(filename):
            with open(filename, 'r') as f:
                for line in f:
                    sl = line.split(':')
                    if len(sl) >= 2:
                        lines.append(
                            GPS.FileLocation(GPS.File(sl[0]),
                                             int(sl[1]),
                                             1))
        return lines

    def handle_entry(self, unit, list):
        """code do handle one entry of the JSON file. See [parsejson] for the
           details of the format.
        """
        for entry in list:
            if 'msg_id' in entry:
                full_id = unit, entry['msg_id']
                self.extra_info[full_id] = entry

    def parsejson(self, unit, file):
        """parse the json file "file", which belongs to unit "unit" and fill
           the "extra_info" mapping for any entry.
           The json file, if it exists and is a valid JSON value, is a dict
           with two entries "flow" and "proof" (both entries may be absent).
           Each entry is mapped to a list of dictionaries. Some of these
           dictionaries have the field "msg_id", these dictionaries are extra
           information for the corresponding message for the current unit. For
           those messages, we simply build up a mapping
             (unit, id) -> extra_info
           which is later used to act on this extra information for each
           message.
        """
        if os.path.isfile(file):
            with open(file, 'r') as f:
                try:
                    dict = json.load(f)
                    if 'flow' in dict:
                        self.handle_entry(unit, dict['flow'])
                    if 'proof' in dict:
                        self.handle_entry(unit, dict['proof'])
                except ValueError:
                    pass

    def act_on_extra_info(self, m, extra, objdir, command):
        """act on extra info for the message m. More precisely, if the message
           has a tracefile, add an action to the message which will show/hide
           the corresponding trace, and if the message has manual proof
           information, run the external editor.
        """

        if 'tracefile' in extra and extra['tracefile'] != '':
            tracefile = os.path.join(objdir, extra['tracefile'])
            lines = self.parse_trace_file(tracefile)
            if lines != []:
                m.set_subprogram(lambda m: toggle_trace(m, lines),
                                 'gps-gnatprove-path',
                                 'show path information')
        # We don't want to open hundreds of editors if a Prove All
        # or Prove File was launched with a manual prover.
        # We only open an editor for prove check.
        editor_dialog = "The condition couldn't be verified\n" \
                        + "Would you like to edit the VC file?\n"

        if command.name() == prove_check and 'vc_file' in extra \
           and GPS.MDI.yes_no_dialog(editor_dialog):
            if 'editor_cmd' in extra:
                cmd = extra['editor_cmd']

                try:
                    proc = GPS.Process(cmd,
                                       on_exit=check_proof_after_close,
                                       before_kill=editor_before_kill)
                    proc._proc_msg = m
                    proc._is_killed = False
                except OSError:
                    GPS.MDI.dialog("Editor " + cmd[0]
                                   + " not found\n"
                                   + "Manual proof file saved as: "
                                   + extra['vc_file'] + "\n")
            else:
                GPS.MDI.dialog("No editor configured for this prover\n"
                               + "Manual proof file saved as: "
                               + extra['vc_file'] + "\n")

    def on_exit(self, status, command):
        """When gnatprove has finished, scan through messages to see if extra
           info has been attached to them. If so, parse the .flow and .proof
           files of the corresponding unit to get the extra info, and act on
           the extra info"""

        objdir = os.path.join(
            GPS.Project.root().object_dirs()[0],
            obj_subdir_name)
        #
        imported_units = set()
        for m in GPS.Message.list():
            file = os.path.basename(m.get_file().name())
            text = self.build_msg_full_text(
                file,
                m.get_line(),
                m.get_column(),
                m.get_text())
            if text in self.msg_id:
                id = self.msg_id[text]
                unit = os.path.splitext(file)[0]
                full_id = unit, id
                if unit not in imported_units:
                    self.parsejson(unit, os.path.join(objdir, unit + ".spark"))
                    imported_units.add(unit)
                extra = {}
                if full_id in self.extra_info:
                    extra = self.extra_info[full_id]
                    self.act_on_extra_info(m, extra, objdir, command)

        if self.child is not None:
            self.child.on_exit(status, command)

    def on_stdout(self, text, command):
        """for each gnatprove message, check for a msg_id tag of the form
           [#id] where id is a number. If no such tag is found, just pass the
           text on to the next parser. Otherwise, add a mapping
              msg text -> msg id
           which will be used later (in on_exit) to associate more info to the
           message
        """
        lines = text.splitlines()
        for line in lines:
            m = re.match(self.regex, line)
            if m:
                text = m.group(1)
                self.pass_output(text, command)
                self.msg_id[text] = int(m.group(2))
            else:
                # the line doesn't have any extra info, go on
                self.pass_output(line, command)


def is_file_context(self):
    """This is the context in which "Show Path" may appear."""

    return isinstance(self, GPS.FileContext)


# It's more convenient to define these callbacks outside of the plugin class

def generic_on_analyze(target, args=[]):
    GPS.BuildTarget(target).execute(extra_args=args, synchronous=False)


def on_examine_all(self):
    generic_on_analyze(examine_all)


def on_examine_root_project(self):
    generic_on_analyze(examine_root_project)


def on_examine_file(self):
    generic_on_analyze(examine_file)


def on_prove_all(self):
    generic_on_analyze(prove_all)


def on_prove_root_project(self):
    generic_on_analyze(prove_root_project)


def on_prove_file(self):
    generic_on_analyze(prove_file)


def on_prove_line(self):
    args = []
    lsparg = build_limit_subp_string(self)
    if lsparg is not None:
        args.append(lsparg)
    target = ""
    if isinstance(self, GPS.MessageContext):
        llarg = "--limit-line=" \
                + os.path.basename(self.message().get_file().name()) \
                + ":" + str(self.message().get_line())
        args.append(llarg)
        target = prove_line_loc
    else:
        target = prove_line
    generic_on_analyze(target, args=args)


def on_show_report(self):
    gnatprove_plug.show_report()


def on_clean_up(self):
    generic_on_analyze(clean_up)


def mk_loc_string(sloc):
    locstring = os.path.basename(sloc.file().name()) + ':' \
        + str(sloc.line())
    return locstring


def subprogram_start(cursor):
    """Return the start of the subprogram that we are currently in"""

    # This function has been copied and modified from plug-in "expanded_code"

    blocks = {'CAT_PROCEDURE': 1, 'CAT_FUNCTION': 1}

    if cursor.block_type() == 'CAT_UNKNOWN':
        return None

    min = cursor.buffer().beginning_of_buffer()
    while not cursor.block_type() in blocks and cursor > min:
        cursor = cursor.block_start() - 1

    if cursor > min:
        return cursor.block_start()
    else:
        return None


def compute_subp_sloc(self):
    """Return the location of the declaration of the subprogram that we are
       currently in"""

    try:
        curloc = self.location()
        buf = GPS.EditorBuffer.get(curloc.file(), open=False)
        if buf is not None:
            edloc = buf.at(curloc.line(), curloc.column())
            start_loc = subprogram_start(edloc)
        else:
            return None
    except:
        return None

    if not start_loc:
        return None
    name = edloc.subprogram_name()

    # [subprogram_start] returns the beginning of the line of the
    # definition/declaration. To be able to call GPS.Entity, we need to be
    # closer to the actual subprogram name. We get closer by skipping the
    # keyword that introduces the subprogram (procedure/function/entry etc.)

    start_loc = start_loc.forward_word(1)
    try:
        entity = GPS.Entity(name, start_loc.buffer().file(),
                            start_loc.line(), start_loc.column())
    except:
        return None
    if entity is not None:
        return entity.declaration()
    else:
        return None


def build_limit_subp_string(self):
    loc = compute_subp_sloc(self)
    if loc is not None:
        return '--limit-subp=' + mk_loc_string(loc)
    else:
        return None


def inside_subp_context(self):
    """Return True if the context is inside a subprogram declaration or body"""

    if compute_subp_sloc(self) is not None:
        return 1
    else:
        return 0


def is_file_context(self):
    return isinstance(self, GPS.FileContext)


def generic_action_on_subp(self, action):
    """execute the action on the the given subprogram entity
    """

    # The argument --limit-subp is not defined in the examine_subp/prove_subp
    # build targets, because we have no means of designating the proper
    # location at that point.  A mild consequence is that --limit-subp does not
    # appear in the editable box shown to the user, even if it appears in the
    # uneditable argument list displayed below it.

    arg = build_limit_subp_string(self)
    if arg is not None:
        target = GPS.BuildTarget(action)
        target.execute(extra_args=arg,
                       synchronous=False)


def on_examine_subp(self):
    """execute the "examine subprogram" action on the the given subprogram
       entity
    """

    generic_action_on_subp(self, examine_subp)


def on_prove_subp(self):
    """execute the "prove subprogram" action on the the given subprogram entity
    """

    generic_action_on_subp(self, prove_subp)


class GNATProve_Plugin:

    """Class to contain the main functionality of the GNATProve_Plugin"""

    def __init__(self):
        GPS.parse_xml(xml_gnatprove)
        GPS.parse_xml(xml_gnatprove_menus % {'prefix': prefix})

    def show_report(self):
        """show report produced in gnatprove/gnatprove.out"""

        objdirs = GPS.Project.root().object_dirs()
        default_objdir = objdirs[0]
        report_file = os.path.join(default_objdir, obj_subdir_name,
                                   report_file_name)

        # if build mode is not the default one, the report file may be found in
        # the parent directory of the current object directory

        if not os.path.exists(report_file):
            if default_objdir.endswith(os.sep):
                default_objdir = default_objdir[:-len(os.sep)]
            default_objdir = os.path.dirname(default_objdir)
            candidate_report_file = \
                os.path.join(default_objdir,
                             obj_subdir_name,
                             report_file_name)

            # if the report file is still not found, leave the original path
            # so that the error message mentions this one

            if os.path.exists(candidate_report_file):
                report_file = candidate_report_file
        buf = GPS.EditorBuffer.get(GPS.File(report_file))
        v = buf.current_view()
        GPS.MDI.get_by_child(v).raise_window()

# Manual proof


class UnknownVCError(Exception):

    def __init__(self, msg):
        self.msg = msg

vc_msg_dict = {
    'divide by zero might fail': 'VC_DIVISION_CHECK',
    'array index check might fail': 'VC_INDEX_CHECK',
    'overflow check might fail': 'VC_OVERFLOW_CHECK',
    'range check might fail': 'VC_RANGE_CHECK',
    'length check might fail': 'VC_LENGTH_CHECK',
    'discriminant check might fail': 'VC_DISCRIMINANT_CHECK',
    'tag check might fail': 'VC_TAG_CHECK',
    'initial condition might fail': 'VC_INITIAL_CONDITION',
    'precondition might fail': 'VC_PRECONDITION',
    'precondition of main program might fail': 'VC_PRECONDITION_MAIN',
    'postcondition might fail': 'VC_POSTCONDITION',
    'refined postcondition might fail': 'VC_REFINED_POST',
    'contract case might fail': 'VC_CONTRACT_CASE',
    'contract cases might not be disjoint': 'VC_DISJOINT_CONTRACT_CASES',
    'contract cases might not be complete': 'VC_COMPLETE_CONTRACT_CASES',
    'loop invariant might fail': 'VC_LOOP_INVARIANT',
    'loop invariant might fail in first iteration': 'VC_LOOP_INVARIANT_INIT',
    'loop invariant might fail after first iteration':
    'VC_LOOP_INVARIANT_PRESERV',
    'loop variant might fail': 'VC_LOOP_VARIANT',
    'assertion might fail': 'VC_ASSERT',
    'exception might be raised': 'VC_RAISE'
}


def is_prove_warning(msg):
    for vc_warn in vc_msg_dict.keys():
        if msg.get_text()[9:].startswith(vc_warn):
            return True
    return False


def get_line_warn(context):
    def msg_filter(msg):
        return msg.get_line() == context.location().line() \
            and is_prove_warning(msg)
    return filter(msg_filter, GPS.Message.list(file=context.file()))


def prove_check_context(context):
    if isinstance(context, GPS.FileContext):
        if isinstance(context, GPS.MessageContext):
            context._loc_msg = context.message()
            return is_prove_warning(context._loc_msg)
        else:
            tmp = get_line_warn(context)
            if len(tmp) == 1:
                context._loc_msg = tmp[0]
                return True
            return False
    return False


def get_vc_kind(msg):
    # get rid of "warning:"
    clean_msg = msg.get_text()[9:]

    def best_match(acc, elem):
        if clean_msg.startswith(elem):
            if not acc or len(acc) < len(elem):
                return elem
            else:
                return acc
        else:
            return acc

    msg_key = reduce(best_match,
                     vc_msg_dict.keys(), None)
    if not msg_key:
        raise UnknownVCError(clean_msg)
    return vc_msg_dict[msg_key]


def limit_line_option(msg, vc_kind):
    return "--limit-line=" + os.path.basename(msg.get_file().name()) \
                           + ":" + str(msg.get_line()) \
                           + ":" + str(msg.get_column()) \
                           + ":" + vc_kind


def on_prove_check(context):
    msg = context._loc_msg
    vc_kind = get_vc_kind(msg)
    llarg = limit_line_option(msg, vc_kind)
    GPS.BuildTarget(prove_check).execute(extra_args=[llarg],
                                         synchronous=False)

# Check for GNAT toolchain: gnatprove

gnatprove = os_utils.locate_exec_on_path(toolname)

if gnatprove:

    # Check for SPARK 2005 toolchain: spark

    spark2005 = os_utils.locate_exec_on_path('spark')

    # Rename menu into "SPARK 2014" if there is already a menu "SPARK" for
    # SPARK 2005 toolset.

    if spark2005:
        prefix = 'SPARK 2014'
        menu_prefix = '/' + prefix

    example_root = \
        os.path.dirname(os.path.dirname(gnatprove)).replace('\\', '/') \
        + '/share/examples/spark'
    xml_gnatprove_menus = \
        xml_gnatprove_menus.replace('@EXAMPLE@', example_root)

    gnatprove_plug = GNATProve_Plugin()
