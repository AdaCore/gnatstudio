xml_gnatsas = (
    """<?xml version="1.0"?>
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

    <action name="gnatsas_example_inspector_by_example" category=""
            show-command="false" output="none">
      <shell>Project.load"""
    """ "{example}/inspector_by_example/inspector_by_example.gpr"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/GNATSAS/Examples</title>
      <menu action="gnatsas_example_inspector_by_example">
        <title>Inspector by Example</title>
      </menu>
    </submenu>

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
      name="Output_Dir"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Output directory"
      hide_in="wizard gnatname_wizard library_wizard"
      description="GNATSAS output directory to use for this project.">
      <string type="directory"/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Review_File"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Review file"
      hide_in="wizard gnatname_wizard library_wizard"
      description="GNATSAS review file to use for this project.">
      <string type="file"/>
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
      label="Custom 'pending' statuses"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Custom message review statuses for the 'pending' kind"
      list="true">
      <string/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Not_A_Bug_Status"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Custom 'not a bug' statuses"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Custom message review statuses for the 'not a bug' kind"
      list="true">
      <string/>
    </project_attribute>

    <project_attribute
      package="Analyzer"
      name="Bug_Status"
      editor_page="GNATSAS"
      editor_section="GNATSAS configuration"
      label="Custom 'bug' statuses"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Custom message review statuses for the 'bug' kind"
      list="true">
      <string/>
    </project_attribute>

    <tool name="GNATSAS analyze" package="Analyzer" index="analyze">
      <language>Ada</language>
      <switches columns="2" lines="3">
        <spin label="Multiprocessing" switch="-j" min="0" max="1000"
              default="0" separator="" column="2"
              tip="Specify the number of processes to generate SCIL files and analyze files (0 means use as
many cores as available on the machine)." />
         <check label="Enable GNAT warnings" switch="--gnat"
                switch-off="--no-gnat" default="on"
                column="2" tip="Enable/disable launching GNAT front-end and collecting its warnings."/>
         <check label="Enable Infer" switch="--infer" switch-off="--no-infer" default="on"
                column="2" tip="Enable/disable Infer analysis (enabled by default)."/>
         <check label="Enable Inspector" switch="--inspector" switch-off="--no-inspector" default="on"
                column="2" tip="Enable/disable Inspector analysis (enabled by default)."/>
         <check label="Enable GNATcheck" switch="--gnatcheck" switch-off="--no-gnatcheck" default="on"
                column="2" tip="Enable/disable GNATcheck analysis (enabled by default)."/>
         <hidden switch="-U" separator=" "/>
      </switches>
    </tool>

    <tool name="GNATSAS inspector" package="Analyzer" index="inspector">
      <language>Ada</language>
      <switches columns="2" lines="3">
        <check label="Ignore representation clauses" switch="-gnatI"
               column="1"
               tip="Perform analysis as if representation clauses had been stripped from the source code." />
        <check label="Unconstrained float overflow" switch="-gnateF"
               column="2"
               tip="Check for overflow on unconstrained floating point types."/>
      </switches>
    </tool>

    <builder-mode name="gnatsas">
      <subdir>gnatsas</subdir>
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

    <target-model name="gnatsas_msg_reader_html" category="">
       <description>Generate GNAT SAS HTML report</description>
       <command-help>{report_help}</command-help>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
    </target-model>

    <target model="gnatsas_msg_reader_html" category="GNATSAS"
            name="Generate HTML Report" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-compile-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>html</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <output-parsers>
         {output_parsers}
       </output-parsers>
    </target>

    <target-model name="gnatsas_msg_reader_csv" category="">
       <description>Generate GNAT SAS CSV report</description>
       <command-help>{report_help}</command-help>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="2" lines="4">
         <check label="Show annotations" switch="--show=kind+annotation"
               column="1"
               tip="Show GNAT SAS annotations in addition to messages (hidden by default)."/>
         <check label="Show info messages" switch="--show=kind+info"
               column="1"
               tip="Show messages of kind info (hidden by default)."/>
         <check label="Show removed" switch="--show=age-removed"
               column="1"
               tip="Show messages removed from the baseline run (hidden by default)."/>
         <check label="Hide low messages" switch="--show=rank-low"
               column="1"
               tip="Hide messages that have a low ranking (shown by default)."/>
         <field
               column="2"
               label="Compare with"
               switch="--compare-with"
               separator=" "
               as-file="true"
               file-filter="*.sam"
               tip="Specify a SAM file with which to compare the current analysis run.
 These files are located under the 'obj_dir/gnatsas/P.outputs' directory."/>
       </switches>
    </target-model>

    <target model="gnatsas_msg_reader_csv" category="GNATSAS"
            name="Generate CSV Report" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-compile-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>csv</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--out=gnatsas.csv</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <output-parsers>
        {output_parsers}
       </output-parsers>
    </target>

    <target model="gprclean" category="GNATSAS" name="Clean GNATSAS Messages"
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
          <arg>%subdirsarg</arg>
       </command-line>
    </target>

    <target-model name="gnatsas_regenerate_report" category="">
       <description>regenerate report</description>
       <command-help>{report_help}</command-help>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>gnat-studio</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--show-annotations</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="3" lines="2">
         <field label="Compare with"
                tip="Specify a SAM file for a one-off comparison with the selected
analysis run (the last run of the specified timeline, or the current run if no timeline
was specified). Generated SAM files are located under the 'obj_dir/gnatsas/P.outputs'
directory (or the directory specified by the 'Output_Dir' attribute of the 'Analyzer' package).
If no file is selected, the baseline of the selected run is used for comparison."
                switch="--compare-with"
                separator=" "
                as-file="true"
                file-filter="*.sam"/>
         <combo label="Timeline" switch="--timeline" noswitch="default"
               separator="=" column="1"
              tip="Timeline for which to regenerate a report. If &quot;--timeline&quot;
is not specified, the timeline of the last analysis is used. To display the report for
a different timeline (e.g. for the analysis of a given file), specify
&quot;--timeline=&lt;timeline_name&gt;&quot; in the command-line." >
            <combo-entry label="Timeline from last analysis" value="default" />
            <combo-entry label="fast (analysis of the project in fast mode)" value="fast" />
            <combo-entry label="deep (analysis of the project in deep mode)" value="deep" />
         </combo>
         <hidden switch="--progress-bar=gnat-studio" separator=" "/>
       </switches>
    </target-model>

    <target model="gnatsas_regenerate_report" category="GNATSAS"
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
          <arg>--show-annotations</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <output-parsers>
        {output_parsers}
       </output-parsers>
    </target>

    <target-model name="gnatsas_load_report" category="">
       <description>load sam report</description>
       <command-help>{report_help}</command-help>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>gnat-studio</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>%Fp</arg>
          <arg>--show-annotations</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
    </target-model>

    <target model="gnatsas_load_report" category="GNATSAS"
            name="Load GNATSAS Report" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>gnat-studio</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>%Fp</arg>
          <arg>--show-annotations</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <output-parsers>
        {output_parsers}
       </output-parsers>
    </target>

    <target-model name="gnatsas_analyze" category="">
       <description>Review code with GNATSAS</description>
       <command-help>{analyze_help}</command-help>
       <command-line>
          <arg>gnatsas</arg>
          <arg>analyze</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="2" lines="7">
         <spin label="Multiprocessing" switch="-j" min="0" max="1000"
               default="0" separator="" column="1"
               tip="Specify the number of processes to generate SCIL files and analyze files (0 means use as
many cores as available on the machine)."/>
         <combo label="Analysis mode" switch="--mode" noswitch="default"
               separator="=" column="1"
               tip="Set the analysis mode to either fast (the default) or deep." >
            <combo-entry label="default mode" value="default" />
            <combo-entry label="fast" value="fast" />
            <combo-entry label="deep" value="deep" />
         </combo>
         <check label="Root project only" switch="--no-subprojects"
                column="2" tip="GNAT SAS will only analyze the source files associated with the root project file (presuming your application uses a tree of project files to represent all of its constituents)."/>
         <check label="Force analysis" switch="-f" column="2"
          tip="Force analysis of all files (disable incrementality)."/>
         <check label="Enable GNAT warnings" switch="--gnat"
                default="on"
                column="2" tip="Enable/disable launching GNAT front-end and collecting its warnings."/>
         <check label="Enable Infer" switch="--infer" switch-off="--no-infer" default="on"
                column="2" tip="Enable/disable Infer analysis (enabled by default)."/>
         <check label="Enable Inspector" switch="--inspector" switch-off="--no-inspector" default="on"
                column="2" tip="Enable/disable Inspector analysis (enabled by default)."/>
         <check label="Enable GNATcheck" switch="--gnatcheck" switch-off="--no-gnatcheck" default="on"
                column="2" tip="Enable/disable GNATcheck analysis (enabled by default)."/>
         <check label="Keep going" switch="--keep-going"
                column="2" tip="If set, do not stop on failure of an analysis engine, but only if all analysis engines fail."/>
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
          <arg>--progress-bar=gnat-studio</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <output-parsers>
        {output_parsers}
       </output-parsers>
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
          <arg>--progress-bar=gnat-studio</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <output-parsers>
        {output_parsers}
       </output-parsers>
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
          <arg>--progress-bar=gnat-studio</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <output-parsers>
        {output_parsers}
       </output-parsers>
    </target>

    <target-model name="gnatsas_report" category="GNATSAS">
       <description>Generate GNATSAS Report</description>
       <command-line>
          <arg>gnatsas</arg>
          <arg>report</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>%subdirsarg</arg>
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
          <arg>--show-annotations</arg>
          <arg>%subdirsarg</arg>
       </command-line>
    </target>

    <target-model name="gnatsas_review" category="GNATSAS">
       <description>Add annotation to GNATSAS report</description>
       <command-line>
          <arg>gnatsas</arg>
          <arg>review</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>%subdirsarg</arg>
          <arg>--gs-request</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
    </target-model>

    <target model="gnatsas_review" category="GNATSAS"
            name="GNATSAS Review" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>review</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>%subdirsarg</arg>
          <arg>--gs-request</arg>
       </command-line>
       <output-parsers>
        {output_parsers}
       </output-parsers>
    </target>

    <target-model name="gnatsas_baseline" category="GNATSAS">
       <description>Modify GNATSAS baselines</description>
       <command-line>
          <arg>gnatsas</arg>
          <arg>baseline</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
    </target-model>

    <target model="gnatsas_baseline" category="GNATSAS"
            name="GNATSAS Baseline Bump" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>baseline</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--bump-baseline</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <output-parsers>
        {output_parsers}
       </output-parsers>
    </target>

    <target model="gnatsas_baseline" category="GNATSAS"
            name="GNATSAS Baseline Set Baseline" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>baseline</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--set-baseline=%Fp</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <output-parsers>
        {output_parsers}
       </output-parsers>
    </target>

    <target model="gnatsas_baseline" category="GNATSAS"
            name="GNATSAS Baseline Set Current" messages_category="GNATSAS">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatsas</arg>
          <arg>baseline</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--set-current=%Fp</arg>
          <arg>%subdirsarg</arg>
       </command-line>
       <output-parsers>
        {output_parsers}
       </output-parsers>
    </target>
  </GNATSAS>
"""
)
