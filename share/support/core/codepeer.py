"""
This file provides support for using the CodePeer tool.

CodePeer is a static analysis tool for Ada code.
This tool allows the user to perform an automatic code review of
a project and integrates its output into GPS.
See menu CodePeer.
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import os_utils
import os.path

xml_codepeer = """<?xml version="1.0"?>
  <CODEPEER>
    <doc_path>{root}/share/doc/codepeer</doc_path>

    <documentation_file>
      <name>codepeer_ug.html</name>
      <descr>CodePeer User's Guide</descr>
      <category>CodePeer</category>
      <menu before="About">/Help/CodePeer/CodePeer User's Guide</menu>
    </documentation_file>

    <documentation_file>
      <name>users_guide/html/index.html</name>
      <descr>CodePeer User's Guide</descr>
      <category>CodePeer</category>
      <menu before="About">/Help/CodePeer/CodePeer User's Guide</menu>
    </documentation_file>

    <documentation_file>
      <name>codepeer_tutorial.html</name>
      <descr>CodePeer Tutorial</descr>
      <category>CodePeer</category>
      <menu before="About">/Help/CodePeer/CodePeer Tutorial</menu>
    </documentation_file>

    <documentation_file>
      <name>tutorial/html/index.html</name>
      <descr>CodePeer Tutorial</descr>
      <category>CodePeer</category>
      <menu before="About">/Help/CodePeer/CodePeer Tutorial</menu>
    </documentation_file>

    <action name="codepeer_example_backpack" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/backpack/backpack.gpr"</shell>
      <shell>Editor.edit "backpack.ads"</shell>
      <shell>Editor.edit "backpack.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_backpack">
        <title>Backpack</title>
      </menu>
    </submenu>

    <action name="codepeer_example_coach" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/coach/coach.gpr"</shell>
      <shell>Editor.edit "coach.ads"</shell>
      <shell>Editor.edit "coach.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_coach">
        <title>Coach</title>
      </menu>
    </submenu>

    <action name="codepeer_example_codepeer_by_example" category=""
            show-command="false" output="none">
      <shell>Project.load""" \
        """ "{example}/codepeer_by_example/codepeer_by_example.gpr"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_codepeer_by_example">
        <title>CodePeer by Example</title>
      </menu>
    </submenu>

    <action name="codepeer_example_conductor" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/conductor/conductor.gpr"</shell>
      <shell>Editor.edit "score.ads"</shell>
      <shell>Editor.edit "conductor.ads"</shell>
      <shell>Editor.edit "conductor.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_conductor">
        <title>Conductor</title>
      </menu>
    </submenu>

    <action name="codepeer_example_configs" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/configs/configs.gpr"</shell>
      <shell>Editor.edit "configs.ads"</shell>
      <shell>Editor.edit "configs.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_configs">
        <title>Configs</title>
      </menu>
    </submenu>

    <action name="codepeer_example_contacts" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/contacts/contacts.gpr"</shell>
      <shell>Editor.edit "contacts.ads"</shell>
      <shell>Editor.edit "contacts.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_contacts">
        <title>Contacts</title>
      </menu>
    </submenu>

    <action name="codepeer_example_cruise" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/cruise/cruise.gpr"</shell>
      <shell>Editor.edit "cruise.ads"</shell>
      <shell>Editor.edit "cruise.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_cruise">
        <title>Cruise</title>
      </menu>
    </submenu>

    <action name="codepeer_example_false_tests" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/false_tests/example.gpr"</shell>
      <shell>Editor.edit "false_tests.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_false_tests">
        <title>False Tests</title>
      </menu>
    </submenu>

    <action name="codepeer_example_pilot" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/pilot/pilot.gpr"</shell>
      <shell>Editor.edit "pilot.ads"</shell>
      <shell>Editor.edit "pilot.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_pilot">
        <title>Pilot</title>
      </menu>
    </submenu>

    <action name="codepeer_example_radar" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/radar/radar.gpr"</shell>
      <shell>Editor.edit "radar.ads"</shell>
      <shell>Editor.edit "radar.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_radar">
        <title>Radar</title>
      </menu>
    </submenu>

    <action name="codepeer_example_uninitialized" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/uninitialized/uninitialized.gpr"</shell>
      <shell>Editor.edit "adt.ads"</shell>
      <shell>Editor.edit "adt.adb"</shell>
      <shell>Editor.edit "uninit.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_uninitialized">
        <title>Uninitialized</title>
      </menu>
    </submenu>

    <action name="codepeer_example_voting" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/voting/voting.gpr"</shell>
      <shell>Editor.edit "voting.ads"</shell>
      <shell>Editor.edit "voting.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_voting">
        <title>Voting</title>
      </menu>
    </submenu>

    <project_attribute
      package="CodePeer"
      name="Output_Directory"
      editor_page="CodePeer"
      editor_section="CodePeer configuration"
      label="Output directory"
      hide_in="wizard gnatname_wizard library_wizard"
      description="CodePeer output directory to use for this project.">
      <string type="directory"/>
    </project_attribute>

    <project_attribute
      package="CodePeer"
      name="Database_Directory"
      editor_page="CodePeer"
      editor_section="CodePeer configuration"
      label="Database directory"
      hide_in="wizard gnatname_wizard library_wizard"
      description="CodePeer database directory to use for this project.">
      <string type="directory"/>
    </project_attribute>

    <project_attribute
      package="CodePeer"
      name="CWE"
      editor_page="CodePeer"
      editor_section="CodePeer configuration"
      label="Include CWE ids"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Include CWE ids in output.">
      <choice default="true">False</choice>
      <choice>True</choice>
    </project_attribute>

    <project_attribute
      package="CodePeer"
      name="Message_Patterns"
      editor_page="CodePeer"
      editor_section="CodePeer configuration"
      label="Message patterns"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Alternate MessagePatterns.xml file used for this project.">
      <string type="file"/>
    </project_attribute>

    <project_attribute
      package="CodePeer"
      name="Additional_Patterns"
      editor_page="CodePeer"
      editor_section="CodePeer configuration"
      label="Additional patterns"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Extra MessagePatterns.xml file to use in addition to the
default patterns file.">
      <string type="file"/>
    </project_attribute>

    <project_attribute
      package="CodePeer"
      name="Excluded_Source_Files"
      editor_page="CodePeer"
      editor_section="CodePeer configuration"
      label="Excluded source files"
      hide_in="wizard gnatname_wizard library_wizard"
      description="List of project source file (as base names) which should be
excluded from CodePeer's analysis."
      list="true">
      <string type="file"/>
    </project_attribute>

    <tool name="CodePeer" package="CodePeer" index="">
      <language>Ada</language>
      <switches columns="2" lines="3">
        <check label="Progress bar" switch="-d" column="1"
               tip="Display a progress bar with information about how many
files are left to be compiled" />
        <spin label="Multiprocessing" switch="-j" min="0" max="1000"
              default="1" separator="" column="2"
              tip="Use N processes to carry out the processing (0 means use as
many cores as available on the machine)." />
        <check label="Ignore representation clauses" switch="-gnatI"
               column="1"
               tip="Ignore all representation clauses, useful for generating
SCIL for another architecture" />
        <check label="Unconstrained float overflow" switch="-gnateF"
               column="2"
               tip="Check for overflow on unconstrained floating point types"/>
        <check label="GNAT warnings" switch="-gnatwna" column="1"
               tip="Enable GNAT warnings during SCIL generation" />
        <check label="Generate CodePeer messages" switch="-gnateC" column="2"
               tip="Generate CodePeer messages in compiler format, without
creating/updating the database" />
      </switches>
    </tool>

    <target-model name="generate_scil" category="">
       <description>Generate SCIL files for CodePeer</description>
       <command-line>
          <arg>codepeer-gprbuild</arg>
          <arg>-d</arg>
          <arg>%eL</arg>
          <arg>-P%PP</arg>
          <arg>--codepeer</arg>
          <arg>%X</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="2" lines="3">
         <check label="Progress bar" switch="-d" column="1"
                tip="Display a progress bar with information about how many
files are left to be compiled" />
         <spin label="Multiprocessing" switch="-j" min="0" max="1000"
               default="1" separator="" column="2"
               tip="Use N processes to carry out the processing (0 means use as
many cores as available on the machine)." />
         <check label="Ignore representation clauses" switch="-gnatI"
                column="1"
                tip="Ignore all representation clauses, useful for generating
SCIL for another architecture" />
         <check label="Unconstrained float overflow" switch="-gnateF"
               column="2"
               tip="Check for overflow on unconstrained floating point types"/>
         <check label="GNAT warnings" switch="-gnatwna" column="1"
                tip="Enable GNAT warnings during SCIL generation" />
         <check label="Generate CodePeer messages" switch="-gnateC" column="2"
                tip="Generate CodePeer messages in compiler format, without
creating/updating the database" />
       </switches>
    </target-model>

    <builder-mode name="codepeer">
      <description>Build SCIL for code review</description>
      <subdir>codepeer</subdir>
      <supported-model>generate_scil</supported-model>
      <supported-model>builder</supported-model>
      <supported-model>gnatmake</supported-model>
      <supported-model>gprbuild</supported-model>
      <supported-model filter="--subdirs=">gprclean</supported-model>
      <server>Tools_Server</server>
      <substitutions>
        <substitute src="%builder" dest="codepeer-gprbuild"/>
      </substitutions>
      <extra-args sections="-margs">
        <arg section="-margs">-k</arg>
        <arg section="-margs">--codepeer</arg>
      </extra-args>
    </builder-mode>

    <target-model name="codepeer" category="">
       <description>Review code with codepeer</description>
       <command-line>
          <arg>codepeer</arg>
          <arg>-dbg-on</arg>
          <arg>ide_progress_bar</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="3" lines="4">
         <spin label="Analysis level" switch="-level" min="0" max="4"
               default="3" separator=" " column="1"
               tip="Set the accuracy and speed of the analysis. Use 0 or 1 for
local and quick analysis, 2 for an intermediate analysis, 3 for a relatively
global analysis (within the memory constraints), and 4 to force a global
analysis. See CodePeer documentation for more details."/>
         <check label="Baseline run" switch="-baseline" column="2"
          tip="this run is a baseline run and prior run becomes default cutoff"
         />
         <spin label="Cutoff" switch="-cutoff" min="0" max="100000"
               default="0" separator=" " column="3"
               tip="This run should use id as the cutoff for the base column"/>
         <spin label="Baseline review #" switch="-set-baseline-id" min="0"
               max="100000" default="0" separator=" " column="3"
               tip="This run should use this id for the base column"/>
         <combo label="Messages" switch="-messages" noswitch="default"
               separator=" " column="1"
               tip="Level of verbosity for messages generated by CodePeer" >
            <combo-entry label="Default" value="default" />
            <combo-entry label="Min" value="min" />
            <combo-entry label="Normal" value="normal" />
            <combo-entry label="Max" value="max" />
         </combo>
         <check label="No race condition" switch="-no-race-conditions"
                column="2" tip="Do not perform race conditions analysis" />
         <spin label="Multiprocessing" switch="-jobs" min="0" max="1000"
               default="1" separator=" " column="3"
               tip="Use N processes to carry out the analysis (0 means use as
many cores as available on the machine)." />
         <check label="Root project only" switch="-root-only"
                column="2" tip="Analyze root project only" />
         <check label="Force analysis" switch="-f" column="2"
          tip="Force analysis of all files, ignoring previous run.
Also force the generation of all SCIL files."
         />
         <hidden switch="-dbg-on" separator=" "/>
         <hidden switch="-dbg-off" separator=" "/>
       </switches>
    </target-model>

    <target-model name="codepeer-compiler" category="">
       <description>Review code with codepeer in compiler mode</description>
       <command-line>
          <arg>codepeer</arg>
          <arg>-d</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-compiler-mode</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="3" lines="2">
         <combo label="Messages" switch="-messages" noswitch="default"
               separator=" " column="1"
               tip="Level of verbosity for messages generated by CodePeer" >
            <combo-entry label="Default" value="default" />
            <combo-entry label="Min" value="min" />
            <combo-entry label="Normal" value="normal" />
            <combo-entry label="Max" value="max" />
         </combo>
         <spin label="Multiprocessing" switch="-jobs" min="0" max="1000"
               default="1" separator=" " column="1"
               tip="Use N processes to carry out the analysis (0 means use as
many cores as available on the machine)." />
         <check label="Root project only" switch="-root-only"
                column="2" tip="Analyze root project only" />
         <check label="Force analysis" switch="-f" column="2"
          tip="Force analysis of all files, ignoring previous run.
Also force the generation of all SCIL files."
         />
         <hidden switch="-dbg-on" separator=" "/>
         <hidden switch="-dbg-off" separator=" "/>
       </switches>
    </target-model>

    <target-model name="codepeer_msg_reader" category="">
       <description>Generate messages with codepeer</description>
       <command-line>
          <arg>codepeer</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-output-msg-only</arg>
          <arg>-out</arg>
          <arg>codepeer.out</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="1" lines="2">
         <check label="CSV output" switch="-csv"
               column="1"
               tip="generate output in CSV format, suitable for spreadsheets"/>
         <combo label="Security" switch="-security" noswitch="disabled"
               separator=" " column="1" column-span="2"
               tip="Enable security filter on messages. Disabled: no security
filter (all messages are output). Normal: output only security relevant
messages. Full: same as normal, plus run-time checks related messages">
            <combo-entry label="Disabled" value="disabled" />
            <combo-entry label="Normal" value="normal" />
            <combo-entry label="Full" value="full" />
         </combo>
       </switches>
    </target-model>

    <target-model name="codepeer_bridge" category="">
       <description>Load CodePeer messages</description>
       <iconname>gps-build-all-symbolic</iconname>
       <command-line>
          <arg>gps_codepeer_bridge</arg>
       </command-line>
    </target-model>

    <target model="generate_scil" category="CodePeer" name="Generate SCIL"
            messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer-gprbuild</arg>
          <arg>-d</arg>
          <arg>%eL</arg>
          <arg>-P%PP</arg>
          <arg>--codepeer</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <target model="gprclean" category="CodePeer" name="Remove SCIL"
            messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-clean-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer-gnatclean</arg>
          <arg>-q</arg>
          <arg>-r</arg>
          <arg>%eL</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <target model="codepeer" category="CodePeer" name="Run CodePeer"
            messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-dbg-on</arg>
          <arg>ide_progress_bar</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <target model="codepeer" category="CodePeer" name="Run CodePeer..."
            messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-dbg-on</arg>
          <arg>ide_progress_bar</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <target model="codepeer-compiler" category="CodePeer"
            name="Run CodePeer File"
            messages_category="CodePeer (one file)">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-compile-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-compiler-mode</arg>
          <arg>-file</arg>
          <arg>%fp</arg>
       </command-line>
    </target>

    <target model="codepeer-compiler" category="CodePeer"
            name="Run CodePeer File By File" messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-d</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-compiler-mode</arg>
       </command-line>
    </target>

    <target model="codepeer" category="CodePeer"
            name="Regenerate CodePeer Report" messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-output-only</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <target model="codepeer_msg_reader" category="CodePeer"
            name="Generate CSV Report" messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-compile-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-output-msg-only</arg>
          <arg>-csv</arg>
          <arg>-out</arg>
          <arg>codepeer.csv</arg>
       </command-line>
    </target>

    <target model="codepeer_bridge" category="CodePeer"
            name="CodePeer Bridge" messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gps_codepeer_bridge</arg>
       </command-line>
    </target>
  </CODEPEER>
"""

# Check for GNAT toolchain: codepeer, gps_codepeer_bridge

codepeer = os_utils.locate_exec_on_path("codepeer")


def on_project_changed(hook):
    # Change default build mode to "codepeer"
    # when GNAT is absent and build mode not set for the project
    if not os_utils.locate_exec_on_path("gprconfig"):
        root_project = GPS.Project.root()
        try:
            root_project.get_property("Build-Mode")
        except GPS.Exception:
            GPS.set_build_mode("codepeer")


if codepeer:
    root = os.path.dirname(os.path.dirname(codepeer)).replace('\\', '/')
    example_root = root + '/share/examples/codepeer'
    xml_codepeer = xml_codepeer.format(example=example_root, root=root)
    GPS.parse_xml(xml_codepeer)
    GPS.Hook("project_changed").add(on_project_changed)
