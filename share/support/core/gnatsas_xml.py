xml_gnatsas = """<?xml version="1.0"?>
  <GNATSAS>
    <doc_path>{root}/share/doc/gnatsas</doc_path>

    <documentation_file>
      <name>gnatsas_ug.html</name>
      <descr>GNATSAS User's Guide</descr>
      <category>GNATSAS</category>
      <menu before="About">/Help/GNATSAS/GNATSAS User's Guide</menu>
    </documentation_file>

    <documentation_file>
      <name>users_guide/html/index.html</name>
      <descr>GNATSAS User's Guide</descr>
      <category>GNATSAS</category>
      <menu before="About">/Help/GNATSAS/GNATSAS User's Guide</menu>
    </documentation_file>

    <documentation_file>
      <name>gnatsas_tutorial.html</name>
      <descr>GNATSAS Tutorial</descr>
      <category>GNATSAS</category>
      <menu before="About">/Help/GNATSAS/GNATSAS Tutorial</menu>
    </documentation_file>

    <documentation_file>
      <name>tutorial/html/index.html</name>
      <descr>GNATSAS Tutorial</descr>
      <category>GNATSAS</category>
      <menu before="About">/Help/GNATSAS/GNATSAS Tutorial</menu>
    </documentation_file>

    <action name="gnatsas_example_backpack" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/backpack/backpack.gpr"</shell>
      <shell>Editor.edit "backpack.ads"</shell>
      <shell>Editor.edit "backpack.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_backpack">
        <title>Backpack</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_coach" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/coach/coach.gpr"</shell>
      <shell>Editor.edit "coach.ads"</shell>
      <shell>Editor.edit "coach.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_coach">
        <title>Coach</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_gnatsas_by_example" category=""
            show-command="false" output="none">
      <shell>Project.load""" \
        """ "{example}/gnatsas_by_example/gnatsas_by_example.gpr"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_gnatsas_by_example">
        <title>GNATSAS by Example</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_conductor" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/conductor/conductor.gpr"</shell>
      <shell>Editor.edit "score.ads"</shell>
      <shell>Editor.edit "conductor.ads"</shell>
      <shell>Editor.edit "conductor.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_conductor">
        <title>Conductor</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_configs" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/configs/configs.gpr"</shell>
      <shell>Editor.edit "configs.ads"</shell>
      <shell>Editor.edit "configs.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_configs">
        <title>Configs</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_contacts" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/contacts/contacts.gpr"</shell>
      <shell>Editor.edit "contacts.ads"</shell>
      <shell>Editor.edit "contacts.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_contacts">
        <title>Contacts</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_cruise" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/cruise/cruise.gpr"</shell>
      <shell>Editor.edit "cruise.ads"</shell>
      <shell>Editor.edit "cruise.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_cruise">
        <title>Cruise</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_false_tests" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/false_tests/example.gpr"</shell>
      <shell>Editor.edit "false_tests.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_false_tests">
        <title>False Tests</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_pilot" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/pilot/pilot.gpr"</shell>
      <shell>Editor.edit "pilot.ads"</shell>
      <shell>Editor.edit "pilot.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_pilot">
        <title>Pilot</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_radar" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/radar/radar.gpr"</shell>
      <shell>Editor.edit "radar.ads"</shell>
      <shell>Editor.edit "radar.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_radar">
        <title>Radar</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_uninitialized" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/uninitialized/uninitialized.gpr"</shell>
      <shell>Editor.edit "adt.ads"</shell>
      <shell>Editor.edit "adt.adb"</shell>
      <shell>Editor.edit "uninit.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_uninitialized">
        <title>Uninitialized</title>
      </menu>
    </submenu>

    <action name="gnatsas_example_voting" category=""
            show-command="false" output="none">
      <shell>Project.load "{example}/voting/voting.gpr"</shell>
      <shell>Editor.edit "voting.ads"</shell>
      <shell>Editor.edit "voting.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_voting">
        <title>Voting</title>
      </menu>
    </submenu>

    <project_attribute
      package="Analyzer"
      name="Output_Directory"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Output directory"
      hide_in="wizard gnatname_wizard library_wizard"
      description="GNATSAS output directory to use for this project.">
      <string type="directory"/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Database_Directory"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Database directory"
      hide_in="wizard gnatname_wizard library_wizard"
      description="GNATSAS database directory to use for this project.">
      <string type="directory"/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Server_URL"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="GNATSAS Server URL"
      hide_in="wizard gnatname_wizard library_wizard"
      description="URL used to connect to the GNATSAS server to access
the GNATSAS database remotely.">
      <string/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="CWE"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Include CWE ids"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Include CWE ids in output.">
      <choice default="true">False</choice>
      <choice>True</choice>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Message_Patterns"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Message patterns"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Alternate MessagePatterns.xml file used for this project.">
      <string type="file"/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Additional_Patterns"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Additional patterns"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Extra MessagePatterns.xml file to use in addition to the
default patterns file.">
      <string type="file"/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Excluded_Source_Files"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Excluded source files"
      hide_in="wizard gnatname_wizard library_wizard"
      description="List of project source file (as base names) which should be
excluded from GNATSAS's analysis."
      list="true">
      <string type="file"/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Pending_Status"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Custom 'pending' status"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Custom message review status for the 'pending' category"
      list="true">
      <string/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Not_A_Bug_Status"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Custom 'not a bug' status"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Custom message review status for the 'not a bug' category"
      list="true">
      <string/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Bug_Status"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Custom 'bug' status"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Custom message review status for the 'bug' category"
      list="true">
      <string/>
    </project_attribute>

    <tool name="GNATSAS" package="Analyzer" index="">
      <language>Ada</language>
      <switches columns="2" lines="3">
        <check label="Progress bar" switch="-d" column="1"
               tip="Display a progress bar with information about how many
files are left to be compiled" />
        <spin label="Multiprocessing" switch="-j" min="0" max="1000"
              default="0" separator="" column="2"
              tip="Use N processes to carry out the processing (0 means use as
many cores as available on the machine)." />
        <check label="Ignore representation clauses" switch="-gnatI"
               column="1"
               tip="Ignore all representation clauses, useful for generating
SCIL for another architecture" />
        <check label="Unconstrained float overflow" switch="-gnateF"
               column="2"
               tip="Check for overflow on unconstrained floating point types"/>
         <combo label="Analysis level" switch="--level" noswitch="default"
               separator=" " column="1"
               tip="Set the accuracy and speed of the analysis. Use 0 or 1 for
local and quick analysis, 2 for an intermediate analysis, 3 for a relatively
global analysis (within the memory constraints), and 4 to force a global
analysis. See GNATSAS documentation for more details." >
            <combo-entry label="default level" value="default" />
            <combo-entry label="0" value="0" />
            <combo-entry label="1" value="1" />
            <combo-entry label="2" value="2" />
            <combo-entry label="3" value="3" />
            <combo-entry label="4" value="4" />
         </combo>
         <spin label="Cutoff" switch="--cutoff" min="0" max="100000"
               default="0" separator=" " column="2"
               tip="This run should use id as the cutoff for the base column"/>
         <combo label="Messages" switch="--messages" noswitch="default"
               separator=" " column="1"
               tip="Level of verbosity for messages generated by GNATSAS" >
            <combo-entry label="Default" value="default" />
            <combo-entry label="Min" value="min" />
            <combo-entry label="Normal" value="normal" />
            <combo-entry label="Max" value="max" />
         </combo>
         <spin label="Baseline review #" switch="--set-baseline-id" min="0"
               max="100000" default="0" separator=" " column="2"
               tip="This run should use this id for the base column"/>
         <hidden switch="-U" separator=" "/>
      </switches>
    </tool>

    <target-model name="generate_scil" category="">
       <description>Generate SCIL files for GNATSAS</description>
       <command-line>
          <arg>codepeer-gprbuild</arg>
          <arg>-d</arg>
          <arg>%eL</arg>
          <arg>-P%PP</arg>
          <arg>--gnatsas</arg>
          <arg>%X</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="2" lines="3">
         <check label="Progress bar" switch="-d" column="1"
                tip="Display a progress bar with information about how many
files are left to be compiled" />
         <spin label="Multiprocessing" switch="-j" min="0" max="1000"
               default="0" separator="" column="2"
               tip="Use N processes to carry out the processing (0 means use as
many cores as available on the machine)." />
         <check label="Ignore representation clauses" switch="-gnatI"
                column="1"
                tip="Ignore all representation clauses, useful for generating
SCIL for another architecture" />
         <check label="Unconstrained float overflow" switch="-gnateF"
               column="2"
               tip="Check for overflow on unconstrained floating point types"/>
         <check label="Generate GNATSAS messages" switch="-gnateC" column="1"
                tip="Generate GNATSAS messages in compiler format, without
creating/updating the database" />
       </switches>
    </target-model>

    <target model="generate_scil" category="GNATSAS" name="Generate SCIL"
            messages_category="GNATSAS">
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
          <arg>--gnatsas</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <builder-mode name="gnatsas">
      <description>Build SCIL for code review</description>
      <subdir>gnatsas</subdir>
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

    <target-model name="gnatsas_msg_reader" category="">
       <description>Generate gnatsas messages</description>
       <command-help>{help}</command-help>
       <command-line>
          <arg>gnatsas</arg>
          <arg>analyze</arg>
          <arg>-d</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--output-msg-only</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="2" lines="5">
         <check label="Show annotations" switch="--show-annotations"
               column="1"
               tip="Show GNATSAS annotations in addition to messages"/>
         <check label="Show informationals" switch="--show-info"
               column="1"
               tip="Show GNATSAS informational messages"/>
         <check label="Show removed" switch="--show-removed"
               column="1"
               tip="Show messages removed from baseline run"/>
         <check label="Hide low messages" switch="--hide-low"
               column="1"
               tip="Do not generate messages ranked low"/>
         <check label="CWE" switch="--cwe"
               column="1"
               tip="Include CWE ids in message output"/>

         <spin label="Current" switch="--current" min="0" max="100000"
               default="0" separator=" " column="2"
               tip="Override current run id"/>
         <spin label="Cutoff" switch="--cutoff" min="0" max="100000"
               default="0" separator=" " column="2"
               tip="Override baseline run id"/>

         <check label="CSV output" switch="--csv"
               column="2"
               tip="generate output in CSV format, suitable for spreadsheets"/>
         <check label="HTML output" switch="--html-only"
               column="2"
               tip="generate output in HTML format"/>
       </switches>
    </target-model>

    <target-model name="gnatsas_bridge" category="">
       <description>Load GNATSAS messages</description>
       <iconname>gps-build-all-symbolic</iconname>
       <command-line>
          <arg>cpm_gs_bridge</arg>
       </command-line>
    </target-model>

    <target model="gnatsas_bridge" category="GNATSAS"
            name="GNATSAS Bridge" messages_category="GNATSAS Bridge">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>cpm_gs_bridge</arg>
       </command-line>
    </target>

    <target model="gnatsas_msg_reader" category="GNATSAS"
            name="Generate HTML Report" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-compile-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>analyze</arg>
          <arg>-d</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--html-only</arg>
       </command-line>
    </target>

    <target model="gnatsas_msg_reader" category="GNATSAS"
            name="Generate CSV Report" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-compile-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>-d</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--output-msg-only</arg>
          <arg>--csv</arg>
          <arg>--out</arg>
          <arg>gnatsas.csv</arg>
       </command-line>
    </target>

    <target model="gprclean" category="GNATSAS" name="Remove SCIL"
            messages_category="GNATSAS">
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

    <target-model name="gnatsas_output_only" category="">
       <description>regenerate report</description>
       <command-help>{help}</command-help>
       <command-line>
          <arg>gnatsas</arg>
          <arg>--output-only</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="3" lines="2">
         <combo label="Analysis level" switch="--level" noswitch="default"
               separator=" " column="1"
               tip="Analysis level for which you want to regenerate a report.">
            <combo-entry label="Last analysis level" value="default" />
            <combo-entry label="0" value="0" />
            <combo-entry label="1" value="1" />
            <combo-entry label="2" value="2" />
            <combo-entry label="3" value="3" />
            <combo-entry label="4" value="4" />
         </combo>

         <check label="Baseline run" switch="--baseline" column="2"
          tip="this run is a baseline run."/>
         <spin label="Baseline review #" switch="--set-baseline-id" min="0"
               max="100000" default="0" separator=" " column="3"
               tip="This run should use this id for the base column"/>

         <spin label="Current" switch="--current" min="0" max="100000"
               default="0" separator=" " column="2"
               tip="Override current run id"/>
         <spin label="Cutoff" switch="--cutoff" min="0" max="100000"
               default="0" separator=" " column="3"
               tip="This run should use id as the cutoff for the base column"/>

         <hidden switch="--progress-bar=gnatstudio" separator=" "/>
       </switches>
    </target-model>

    <target model="gnatsas_output_only" category="GNATSAS"
            name="Regenerate GNATSAS Report" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>gnat-studio</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--no-analysis</arg>
       </command-line>
    </target>

    <target-model name="gnatsas_analyze" category="">
       <description>Review code with GNATSAS</description>
       <command-help>{help}</command-help>
       <command-line>
          <arg>gnatsas</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="3" lines="6">
         <combo label="Analysis level" switch="--level" noswitch="default"
               separator=" " column="1"
               tip="Set the accuracy and speed of the analysis. Use 0 or 1 for
local and quick analysis, 2 for an intermediate analysis, 3 for a relatively
global analysis (within the memory constraints), and 4 to force a global
analysis. See GNATSAS documentation for more details." >
            <combo-entry label="default level" value="default" />
            <combo-entry label="0" value="0" />
            <combo-entry label="1" value="1" />
            <combo-entry label="2" value="2" />
            <combo-entry label="3" value="3" />
            <combo-entry label="4" value="4" />
         </combo>
         <combo label="Messages" switch="--messages" noswitch="default"
               separator=" " column="1"
               tip="Level of verbosity for messages generated by GNATSAS" >
            <combo-entry label="Default" value="default" />
            <combo-entry label="Min" value="min" />
            <combo-entry label="Normal" value="normal" />
            <combo-entry label="Max" value="max" />
         </combo>

         <spin label="Multiprocessing" switch="-j" min="0" max="1000"
               default="0" separator="" column="3"
               tip="Use N processes to carry out the analysis (0 means use as
many cores as available on the machine)." />
         <field label="Run name"
                tip="Specify the run name."
                switch="--run-name"
                separator=" "/>
         <check label="Root project only" switch="--no-subprojects"
                column="2" tip="Analyze root project only" />
         <check label="Force analysis" switch="-f" column="2"
          tip="Force analysis of all files, ignoring previous run.
Also force the generation of all SCIL files." />
         <check label="Disable Infer" switch="--no-infer" default="off"
                column="2" tip="Disable Infer analysis." />
         <hidden switch="--progress-bar=gnatstudio" separator=" "/>
       </switches>
    </target-model>

    <target model="gnatsas_analyze" category="GNATSAS" name="Run GNATSAS"
            messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>analyze</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <target model="gnatsas_analyze" category="GNATSAS" name="Run GNATSAS..."
            messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>analyze</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <target model="gnatsas_analyze" category="GNATSAS"
            name="Run GNATSAS File"
            messages_category="GNATSAS (one file)">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-compile-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>analyze</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--file</arg>
          <arg>%fp</arg>
       </command-line>
    </target>

    <target-model name="gnatsas_report" category="GNATSAS">
       <description>Generate GNATSAS Report</description>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
    </target-model>

    <target model="gnatsas_report" category="GNATSAS"
            name="Run GNATSAS Report" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>gnat-studio</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
    </target>
  </GNATSAS>
"""
