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
        name="Examine All Sources Action" category="GNATprove" output="none">
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
      name="Prove All Sources Action" category="GNATprove" output="none">
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
        <menu action="Examine All Sources Action">
          <Title>Examine All Sources</Title>
        </menu>
        <menu action="Examine File Action">
          <Title>Examine File</Title>
        </menu>
        <menu><title/></menu>
        <menu action="Prove All Action">
          <Title>Prove All</Title>
        </menu>
        <menu action="Prove All Sources Action">
          <Title>Prove All Sources</Title>
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

    <action name="spark2014_example_adacore_u" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/adacore_u/Overview/overview.gpr"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_adacore_u">
        <title>adacore__u</title>
      </menu>
    </submenu>

    <action name="spark2014_example_autopilot" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/autopilot/test.gpr"</shell>
      <shell>Editor.edit "ap.ads"</shell>
      <shell>Editor.edit "instruments.ads"</shell>
      <shell>Editor.edit "surfaces.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_autopilot">
        <title>autopilot</title>
      </menu>
    </submenu>

    <action name="spark2014_example_binary_search" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/binary_search/test.gpr"</shell>
      <shell>Editor.edit "binary_search.adb"</shell>
      <shell>Editor.edit "binary_search.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_binary_search">
        <title>binary__search</title>
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
        <title>binary__search__unconstrained</title>
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
        <title>database</title>
      </menu>
    </submenu>

    <action name="spark2014_example_euclidian_division" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/euclidian_division/test.gpr"</shell>
      <shell>Editor.edit "linear_div.adb"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_euclidian_division">
        <title>euclidian__division</title>
      </menu>
    </submenu>

    <action name="spark2014_example_evoting" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/evoting/test.gpr"</shell>
      <shell>Editor.edit "evoting.adb"</shell>
      <shell>Editor.edit "evoting.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_evoting">
        <title>evoting</title>
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
        <title>formal__queue</title>
      </menu>
    </submenu>

    <action name="spark2014_example_gnatprove_by_example" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/gnatprove_by_example/test.gpr"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_gnatprove_by_example">
        <title>gnatprove__by__example</title>
      </menu>
    </submenu>

    <action name="spark2014_example_heatingsystem" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/heatingsystem/test.gpr"</shell>
      <shell>Editor.edit "heatingsystem_dfa.adb"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_heatingsystem">
        <title>heatingsystem</title>
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
        <title>intro</title>
      </menu>
    </submenu>

    <action name="spark2014_example_ipstack" category=""
            show-command="false" output="none">
      <shell lang="python">spark2014.load_example_ipstack()</shell>
      <shell>Editor.edit "aip-udp.ads"</shell>
      <shell>Editor.edit "aip-tcp.ads"</shell>
      <shell>Editor.edit "aip-ip.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_ipstack">
        <title>ipstack</title>
      </menu>
    </submenu>

    <action name="spark2014_example_linear_search" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/linear_search/test.gpr"</shell>
      <shell>Editor.edit "linear_search.adb"</shell>
      <shell>Editor.edit "linear_search.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_linear_search">
        <title>linear__search</title>
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
        <title>longest__common__prefix</title>
      </menu>
    </submenu>

    <action name="spark2014_example_natural" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/natural/test.gpr"</shell>
      <shell>Editor.edit "natural_set.adb"</shell>
      <shell>Editor.edit "natural_set.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_natural">
        <title>natural</title>
      </menu>
    </submenu>

    <action name="spark2014_example_n_queens" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/n_queens/test.gpr"</shell>
      <shell>Editor.edit "queen.adb"</shell>
      <shell>Editor.edit "queen.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_n_queens">
        <title>n__queens</title>
      </menu>
    </submenu>

    <action name="spark2014_example_openETCS" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/openETCS/test.gpr"</shell>
      <shell>Editor.edit "section_4_6.adb"</shell>
      <shell>Editor.edit "step_function.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_openETCS">
        <title>openETCS</title>
      </menu>
    </submenu>

    <action name="spark2014_example_patience" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/patience/test.gpr"</shell>
      <shell>Editor.edit "patience.adb"</shell>
      <shell>Editor.edit "patience.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_patience">
        <title>patience</title>
      </menu>
    </submenu>

    <action name="spark2014_example_railway_signaling" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/railway_signaling/test.gpr"</shell>
      <shell>Editor.edit "trains.adb"</shell>
      <shell>Editor.edit "trains.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_railway_signaling">
        <title>railway__signaling</title>
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
        <title>ring__buffer</title>
      </menu>
    </submenu>

    <action name="spark2014_example_search_linked_list" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/search_linked_list/test.gpr"</shell>
      <shell>Editor.edit "lists.adb"</shell>
      <shell>Editor.edit "lists.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_search_linked_list">
        <title>search__linked__list</title>
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
        <title>segway</title>
      </menu>
    </submenu>

    <action name="spark2014_example_sparkskein" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/sparkskein/test.gpr"</shell>
      <shell>Editor.edit "skein.adb"</shell>
      <shell>Editor.edit "skein.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_sparkskein">
        <title>sparkskein</title>
      </menu>
    </submenu>

    <action name="spark2014_example_spark_book" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/spark_book/test.gpr"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_spark_book">
        <title>spark__book</title>
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
        <title>spark__io</title>
      </menu>
    </submenu>

    <action name="spark2014_example_tetris" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/tetris/test.gpr"</shell>
      <shell>Editor.edit "tetris_functional.adb"</shell>
      <shell>Editor.edit "tetris_functional.ads"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_tetris">
        <title>tetris</title>
      </menu>
    </submenu>

    <action name="spark2014_example_thumper" category=""
            show-command="false" output="none">
      <shell lang="python">spark2014.load_example_thumper()</shell>
      <shell>Editor.edit "thumper/src/server/thumper_server.adb"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_thumper">
        <title>thumper</title>
      </menu>
    </submenu>

    <action name="spark2014_example_tokeneer" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/tokeneer/test.gpr"</shell>
      <shell>Editor.edit "tis.adb"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_tokeneer">
        <title>tokeneer</title>
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
label="Proof level"
switch="--level"
noswitch="none"
separator="="
tip="Set the proof level from 0 = faster to 4 = more powerful" >
    <combo-entry label="none" value="none"
                 tip="No level set"/>
    <combo-entry label="0 (fast, one prover)" value="0"
                 tip="Equivalent to --prover=cvc4 --proof=per_check
 --steps=100 --timeout=1"/>
    <combo-entry label="1 (fast, all provers)" value="1"
                 tip="Equivalent to --prover=cvc4,z3,altergo --proof=per_check
 --steps=100 --timeout=1"/>
    <combo-entry label="2 (all provers)" value="2"
                 tip="Equivalent to --prover=cvc4,z3,altergo --proof=per_check
 --steps=1000 --timeout=10"/>
    <combo-entry label="3 (slower, all provers)" value="3"
                 tip="Equivalent to --prover=cvc4,z3,altergo
 --proof=progressive --steps=1000 --timeout=10"/>
    <combo-entry label="4 (slowest, all provers)" value="4"
                 tip="Equivalent to --prover=cvc4,z3,altergo
 --proof=progressive --steps=10000 --timeout=60"/>
</combo>
<combo
label="Proof strategy"
switch="--proof"
noswitch="per_check"
separator="="
tip="Formulas generated for each check (faster) or each path (more precise)" >
    <combo-entry label="one proof per check" value="per_check"
                 tip="Generate one formula per check"/>
    <combo-entry label="one proof per path" value="per_path"
                 tip="Generate one formula per path for each check"/>
    <combo-entry
    label="progressively split"
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
              tip="Use N processes to carry out the processing
 (0 means use as many cores as available on the machine)" />
      </switches>
    </tool>

    <target-model name="gnatprove-examine">
       <description>Target model for GNATprove Examine commands</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--mode=flow</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <persistent-history>False</persistent-history>
    </target-model>

    <target-model name="gnatprove-basic-prove">
       <description>Target model for GNATprove Basic Prove commands
       </description>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s">
         <title column="1" line="1" >General</title>
         <check
           label="Multiprocessing" switch="-j0" column="1"
           tip="Use as many cores as available on the machine"
         />
         <check label="Do not report warnings" switch="--warnings=off"
                column="1" tip="Do not issue warnings at all"
         />
         <check label="Report checks proved" switch="--report=all" column="1"
                tip="Report the status of all checks, including those proved"
         />
         <title column="2" line="1" >Prover</title>
<combo
label="Proof level"
switch="--level"
noswitch="none"
separator="="
column="2"
tip="Set the proof level from 0 = faster to 4 = more powerful" >
    <combo-entry label="none" value="none"
                 tip="No level set"/>
    <combo-entry label="0 (fast, one prover)" value="0"
                 tip="Equivalent to --prover=cvc4 --proof=per_check
 --steps=100 --timeout=1"/>
    <combo-entry label="1 (fast, all provers)" value="1"
                 tip="Equivalent to --prover=cvc4,z3,altergo --proof=per_check
 --steps=100 --timeout=1"/>
    <combo-entry label="2 (all provers)" value="2"
                 tip="Equivalent to --prover=cvc4,z3,altergo --proof=per_check
 --steps=1000 --timeout=10"/>
    <combo-entry label="3 (slower, all provers)" value="3"
                 tip="Equivalent to --prover=cvc4,z3,altergo
 --proof=progressive --steps=1000 --timeout=10"/>
    <combo-entry label="4 (slowest, all provers)" value="4"
                 tip="Equivalent to --prover=cvc4,z3,altergo
 --proof=progressive --steps=10000 --timeout=60"/>
</combo>
       </switches>
       <persistent-history>False</persistent-history>
    </target-model>

    <target-model name="gnatprove-prove">
       <description>Target model for GNATprove Prove commands</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s">
         <title column="1" line="1" >General</title>
         <spin label="Multiprocessing" switch="-j" min="0" max="1000"
          default="1" separator="" column="1"
          tip="Use N processes to carry out the processing
 (0 means use as many cores as available on the machine)" />
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
             <combo-entry label="one proof per check" value="per_check"
                          tip="Generate one formula per check"/>
             <combo-entry label="one proof per path" value="per_path"
                          tip="Generate one formula per path for each check"/>
             <combo-entry
             label="progressively split"
             value="progressive"
             tip="Start with one formula per check,
 then split into paths when needed"/>
         </combo>
         <spin label="Prover timeout" switch="--timeout=" column="2"
                default="1" min="1" max="3600"
                tip="Set the prover timeout (in s) for individual proofs" />
         <field label="Alternate provers" switch="--prover=" column="2"
                tip="Alternate provers to use, instead of CVC4
 followed by Alt-Ergo" />
       </switches>
       <persistent-history>False</persistent-history>
    </target-model>

    <target model="gnatprove-examine" name="Examine All" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-examine" name="Examine All Sources"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-examine"
            name="Examine Single File" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-basic-prove" name="Basic Prove All"
     category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-basic-prove" name="Basic Prove All Sources"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-prove" name="Prove All Sources"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-basic-prove" name="Basic Prove File"
     category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-prove" name="Prove File" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-basic-prove" name="Basic Prove Subprogram"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-prove" name="Prove Subprogram"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-basic-prove" name="Basic Prove Line"
     category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-prove" name="Prove Line" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-basic-prove" name="Basic Prove Line Location"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-prove" name="Prove Line Location"
            category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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
          <arg>%X</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <persistent-history>False</persistent-history>
    </target-model>

    <target model="gnatprove_clean" name="Clean Proofs" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--clean</arg>
       </command-line>
    </target>

    <target model="gnatprove-basic-prove" name="Basic Prove Check"
     category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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

    <target model="gnatprove-prove" name="Prove Check" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
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
examine_root_project = 'Examine All Sources'
examine_file = 'Examine Single File'
examine_subp = 'Examine Subprogram'

# proof targets when user profile is 'Basic'

basic_prove_all = 'Basic Prove All'
basic_prove_root_project = 'Basic Prove All Sources'
basic_prove_file = 'Basic Prove File'
basic_prove_subp = 'Basic Prove Subprogram'
basic_prove_line = 'Basic Prove Line'
basic_prove_line_loc = 'Basic Prove Line Location'
basic_prove_check = 'Basic Prove Check'

# proof targets when user profile is 'Advanced'

advanced_prove_all = 'Prove All'
advanced_prove_root_project = 'Prove All Sources'
advanced_prove_file = 'Prove File'
advanced_prove_subp = 'Prove Subprogram'
advanced_prove_line = 'Prove Line'
advanced_prove_line_loc = 'Prove Line Location'
advanced_prove_check = 'Prove Check'


# getters for proof target depending on user profile


def prove_all():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_all
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_all


def prove_root_project():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_root_project
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_root_project


def prove_file():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_file
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_file


def prove_subp():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_subp
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_subp


def prove_line():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_line
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_line


# used to launch Prove Line from Location View
def prove_line_loc():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_line_loc
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_line_loc


# in case of manual provers, prove_check is
# the only one allowed to open editors
def prove_check():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_check
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_check

show_report = 'Show Report'
clean_up = 'Clean Proofs'
check_msg_prefix = 'medium: '

Default_Trace_Color = "#00ffff"  # "aqua" (light blue)
Overlay_Name = "Gnatprove_Trace_Overlay"
Ce_Spec_Lines_Name = "Gnatprove_Ce_Special_Lines"
Ce_Highlighting = "Gnatprove_Ce_Highlighting"
Ce_Highlighting_Color = "#dddddd"  # light grey

User_Profile_Pref_Name = 'SPARK/user_profile'

GPS.Preference(User_Profile_Pref_Name).create(
    'User profile',
    'enum',
    'Basic user profile for simple proof panel,' +
    ' advanced user profile for more complex proof panel.',
    0,
    'Basic',
    'Advanced')

Color_Pref_Name = 'SPARK/color_trace'

GPS.Preference(Color_Pref_Name).create(
    'Highlight color for trace',
    'color',
    'color used to highlight trace lines.' +
    ' You must restart GPS to take changes into account.',
    Default_Trace_Color)


def get_example_root():
    """retrieve the full path to the directory containing the examples as
       installed locally.
    """
    return os.path.dirname(os.path.dirname(gnatprove)).replace('\\', '/') \
        + '/share/examples/spark'


def update_project_path(paths):
    """update GPR_PROJECT_PATH with the paths given as input, taking into account
       the existing setting of both GPR_PROJECT_PATH and ADA_PROJECT_PATH.
    """
    import os
    os.environ["GPR_PROJECT_PATH"] = \
        ':'.join(paths) + ':' + \
        os.environ["GPR_PROJECT_PATH"] + ':' + \
        os.environ["ADA_PROJECT_PATH"]


def load_example_ipstack():
    """ load IPstack example project, which requires specific code to set
        GPR_PROJECT_PATH and change working directory.
    """
    import os
    ipstack_root = os.path.join(get_example_root(), 'ipstack')
    update_project_path([os.path.join(ipstack_root, 'projects'),
                         os.path.join(ipstack_root, 'projects.native')])
    os.chdir(os.path.join(ipstack_root, 'build'))
    GPS.Project.load(os.path.join(ipstack_root,
                     'projects.native', 'ipstack_dev.gpr'))


def load_example_thumper():
    """ load Thumper example project, which requires specific code to set
        MODE and GPR_PROJECT_PATH environment variables.
    """
    import os
    thumper_root = os.path.join(get_example_root(), 'thumper')
    update_project_path([os.path.join(thumper_root, 'dummy_projects')])
    os.environ["MODE"] = 'Analyze'
    GPS.Project.load(os.path.join(thumper_root,
                     'thumper', 'src', 'thumper.gpr'))


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


def get_ce_special_lines(buf):
    """retrieve the list of special lines for counter-example for a
       buffer. If the buffer hasn't got one yet, create it, add it to the
       buffer, and return it.
    """

    if not hasattr(buf, Ce_Spec_Lines_Name):
        setattr(buf, Ce_Spec_Lines_Name, [])
    return getattr(buf, Ce_Spec_Lines_Name)


def add_ce_special_line(buf, line, text):
    """create a special line in the buffer"""

    line_marker = buf.add_special_line(line+1, text, Ce_Highlighting)
    spec_lines = get_ce_special_lines(buf)
    spec_lines.append(line_marker)
    setattr(buf, Ce_Spec_Lines_Name, spec_lines)


def remove_ce_special_lines(buf):
    """remove all special lines for counter-example in the buffer"""

    line_markers = get_ce_special_lines(buf)
    for line_marker in line_markers:
        buf.remove_special_lines(line_marker, 1)


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
            GPS.BuildTarget(prove_check()).execute(extra_args=[llarg],
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
counterexample = {}


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


def get_ce_text_for_line(line_info):
    """Generates the text to be displayed in counter-example for given
       line."""
    return " and ".join(['%s = %s' % (ce_element["name"], ce_element["value"])
                         for ce_element in line_info])


def get_str_indent(buf, line):
    """Returns a string of white spaces that indents up to the indentation of
       the given line."""
    line_loc = buf.at(line, 1)
    last_column = line_loc.end_of_line().column
    indent = 1
    while line_loc.get_char() == " " and indent < last_column:
        indent += 1
        line_loc = buf.at(line, indent)
    return "" if (indent == last_column) else " " * (indent-1)


def show_ce(ce):
    for file in ce:
        if GPS.File(file).language() == "ada":
            first_sloc = GPS.FileLocation(GPS.File(file),
                                          int(next(iter(ce[file]))),
                                          1)
            buf = GPS.EditorBuffer.get(first_sloc.file())
            goto_location(first_sloc)
            overlay = get_trace_overlay(buf)
            for line in ce[file]:
                text = get_str_indent(buf, int(line)) + "--  " + \
                    get_ce_text_for_line(ce[file][line])
                add_ce_special_line(buf, int(line), text)
                buf.apply_overlay(overlay,
                                  buf.at(int(line), 1),
                                  buf.at(int(line), 1))


def remove_ce(ce):
    for file in ce:
        if GPS.File(file).language() == "ada":
            buf = GPS.EditorBuffer.get(GPS.File(file))
            remove_ce_special_lines(buf)
            overlay = get_trace_overlay(buf)
            buf.remove_overlay(overlay)


def toggle_trace(msg, lines, ce):
    """toggle the trace for the given msg and lines"""
    global trace_msg, trace_lines, counterexample
    if trace_msg is None:
        trace_msg = msg
        trace_lines = lines
        counterexample = ce
        show_trace(lines)
        show_ce(ce)
    elif trace_msg == msg:
        remove_trace(trace_lines)
        remove_ce(counterexample)
        trace_msg = None
    else:
        remove_trace(trace_lines)
        remove_ce(counterexample)
        trace_msg = msg
        trace_lines = lines
        counterexample = ce
        show_trace(lines)
        show_ce(ce)


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
           Note that the returned text must be identical to the text that is
           produced by GNATprove - this  text is used to match the message
           produced by GNATprove and extra information about this message
           stored in *.spark file. See on_stdout and on_exit.
        """
        str_col = str(col)

        # In the message produced by GNATprove, '0' is always prepended
        # to column number that is less than 10. Do it also here.
        if col < 10:
            str_col = '0' + str_col
        return "%s:%s:%s: %s" % (file, line, str_col, text)

    def pass_output(self, text, command):
        """pass the text on to the next output parser"""
        if self.child:
            self.child.on_stdout(text + '\n', command)

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
           has a tracefile or counterexample, add an action to the message
           which will show/hide the corresponding trace or counterexample,
           and if the message has manual proof information, run the external
           editor.
        """

        counterexample = {}
        if 'cntexmp' in extra:
            counterexample = extra['cntexmp']

        lines = []
        if 'tracefile' in extra and extra['tracefile'] != '':
            tracefile = os.path.join(objdir, extra['tracefile'])
            lines = self.parse_trace_file(tracefile)

        if counterexample != {} or lines != []:
            GPS.Editor.register_highlighting(Ce_Highlighting,
                                             Ce_Highlighting_Color,
                                             False)
            m.set_subprogram(lambda m: toggle_trace(m, lines, counterexample),
                             'gps-gnatprove-symbolic',
                             'show counter-example information')
        # We don't want to open hundreds of editors if a Prove All
        # or Prove File was launched with a manual prover.
        # We only open an editor for prove check.
        editor_dialog = "The condition couldn't be verified\n" \
                        + "Would you like to edit the VC file?\n"

        if command.name() == prove_check() and 'vc_file' in extra \
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
                    GPS.MDI.dialog("Editor " + cmd[0] +
                                   " not found\n" +
                                   "Manual proof file saved as: " +
                                   extra['vc_file'] + "\n")
            else:
                GPS.MDI.dialog("No editor configured for this prover\n" +
                               "Manual proof file saved as: " +
                               extra['vc_file'] + "\n")

    def on_exit(self, status, command):
        """When GNATprove has finished, scan through messages to see if extra
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
        """for each GNATprove message, check for a msg_id tag of the form
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
    generic_on_analyze(prove_all())


def on_prove_root_project(self):
    generic_on_analyze(prove_root_project())


def on_prove_file(self):
    generic_on_analyze(prove_file())


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
        target = prove_line_loc()
    else:
        target = prove_line()
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

    blocks = {'CAT_PROCEDURE': 1, 'CAT_FUNCTION': 1, 'CAT_ENTRY': 1}

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

    generic_action_on_subp(self, prove_subp())


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
    # VC_RTE_Kind - run-time checks

    'divide by zero might fail': 'VC_DIVISION_CHECK',
    'array index check might fail': 'VC_INDEX_CHECK',
    'overflow check might fail': 'VC_OVERFLOW_CHECK',
    'range check might fail': 'VC_RANGE_CHECK',
    'length check might fail': 'VC_LENGTH_CHECK',
    'discriminant check might fail': 'VC_DISCRIMINANT_CHECK',
    'interrupt might be reserved': 'VC_INTERRUPT_RESERVED',
    'ceiling priority might not be in Interrupt_Priority':
        'VC_CEILING_INTERRUPT',
    'ceiling priority protocol might not be respected':
        'VC_CEILING_PRIORITY_PROTOCOL',
    'tag check might fail': 'VC_TAG_CHECK',

    # VC_Assert_Kind - assertions

    'initial condition might fail': 'VC_INITIAL_CONDITION',
    'default initial condition might fail': 'VC_DEFAULT_INITIAL_CONDITION',
    'precondition might fail': 'VC_PRECONDITION',
    'call to nonreturning subprogram might be executed': 'VC_PRECONDITION',
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
    'exception might be raised': 'VC_RAISE',

    # VC_LSP_Kind - Liskov Substitution Principle

    'precondition might be stronger than class-wide precondition':
    'VC_WEAKER_PRE',
    'precondition is stronger than the default class-wide precondition '
    'of True':
    'VC_TRIVIAL_WEAKER_PRE',
    'postcondition might be weaker than class-wide postcondition':
    'VC_STRONGER_POST',
    'class-wide precondition might be stronger than overridden one':
    'VC_WEAKER_CLASSWIDE_PRE',
    'class-wide postcondition might be weaker than overridden one':
    'VC_STRONGER_CLASSWIDE_POST'
}


def is_unproved_check_message(msg):
    for vc_warn in vc_msg_dict.keys():
        # get rid of "medium: "
        if msg.get_text()[len(check_msg_prefix):].startswith(vc_warn):
            return True
    return False


def get_line_warn(context):
    def msg_filter(msg):
        return msg.get_line() == context.location().line() \
            and is_unproved_check_message(msg)
    return filter(msg_filter, GPS.Message.list(file=context.file()))


def prove_check_context(context):
    if isinstance(context, GPS.FileContext):
        if isinstance(context, GPS.MessageContext):
            context._loc_msg = context.message()
            return is_unproved_check_message(context._loc_msg)
        else:
            tmp = get_line_warn(context)
            if len(tmp) == 1:
                context._loc_msg = tmp[0]
                return True
            return False
    return False


def get_vc_kind(msg):
    # get rid of "medium: "
    clean_msg = msg.get_text()[len(check_msg_prefix):]

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
    GPS.BuildTarget(prove_check()).execute(extra_args=[llarg],
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

    xml_gnatprove_menus = \
        xml_gnatprove_menus.replace('@EXAMPLE@', get_example_root())

    gnatprove_plug = GNATProve_Plugin()
