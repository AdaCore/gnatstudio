doc = """<?xml version="1.0"?>
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
"""

attributes = """
    <project_attribute
      package="CodePeer"
      name="CPM_Directory"
      editor_page="CodePeer"
      editor_section="CodePeer configuration"
      label="cpm files directory"
      hide_in="wizard gnatname_wizard library_wizard"
      description="CodePeer cpm directory to use for this project.">
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

    <project_attribute
      package="CodePeer"
      name="Pending_Status"
      editor_page="CodePeer"
      editor_section="CodePeer configuration"
      label="Custom 'pending' status"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Custom message review status for the 'pending' category"
      list="true">
      <string/>
    </project_attribute>

    <project_attribute
      package="CodePeer"
      name="Not_A_Bug_Status"
      editor_page="CodePeer"
      editor_section="CodePeer configuration"
      label="Custom 'not a bug' status"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Custom message review status for the 'not a bug' category"
      list="true">
      <string/>
    </project_attribute>

    <project_attribute
      package="CodePeer"
      name="Bug_Status"
      editor_page="CodePeer"
      editor_section="CodePeer configuration"
      label="Custom 'bug' status"
      hide_in="wizard gnatname_wizard library_wizard"
      description="Custom message review status for the 'bug' category"
      list="true">
      <string/>
    </project_attribute>
"""

tool = """
    <tool name="CodePeer" package="CodePeer" index="">
      <language>Ada</language>
      <switches columns="2" lines="3">
        <spin label="Multiprocessing" switch="-j" min="0" max="1000"
              default="0" separator="" column="2"
              tip="Use N processes to carry out the processing (0 means use as
many cores as available on the machine)." />
        <field label="Run name"
               tip="Specify the run name."
               switch="--run-name"
               separator=" "/>
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
analysis. See CodePeer documentation for more details." >
            <combo-entry label="default level" value="default" />
            <combo-entry label="0" value="0" />
            <combo-entry label="1" value="1" />
            <combo-entry label="2" value="2" />
            <combo-entry label="3" value="3" />
            <combo-entry label="4" value="4" />
         </combo>
         <combo label="Messages" switch="--messages" noswitch="default"
               separator=" " column="1"
               tip="Level of verbosity for messages generated by CodePeer" >
            <combo-entry label="Default" value="default" />
            <combo-entry label="Min" value="min" />
            <combo-entry label="Normal" value="normal" />
            <combo-entry label="Max" value="max" />
         </combo>
         <hidden switch="-U" separator=" "/>
      </switches>
    </tool>
"""

from os import listdir
from os.path import basename

def cpms_combo_box(label, switch, tip):
    cpm_dir= GPS.Project.root().get_attribute_as_string (attribute="CPM_Directory", package="CodePeer")
    head = "<combo label='" + label + "' switch='" + switch + "' separator=' ' tip='" + tip + " '>"
    tail = "</combo>"
    body = ""
    for f in os.listdir(cpm_dir):
        if f.endswith(".cpm"):
            body += "<combo-entry label='" + os.path.basename(f) + "' value='" + f + "'/>"

    return head + body + tail

body = """
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

    <target-model name="csv" category="">
       <description>Generate codepeer messages in csv format</description>
       <command-help>{help}</command-help>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="2" lines="5">
         <check label="Show informationals" switch="--show-info"
               column="1"
               tip="Show CodePeer informational messages"/>
         <check label="Hide low messages" switch="--hide-low"
               column="1"
               tip="Do not generate messages ranked low"/>
         <field label="Output file"
                tip="Write csv output to specified file."
                switch="-o"
                separator=" "
                default="codepeer.csv"/>
         <field label="Display cpm"
                tip="Select a cpm file to display instead."
                switch="--current"
                separator=" "
                as-file="true"
                file-filter="*.cpm"/>
         <field label="Compare with"
                tip="Compare the current run with an arbitrary cpm file."
                switch="--compare-with"
                separator=" "
                as-file="true"
                file-filter="*.cpm"/>
       </switches>
    </target-model>

    <target-model name="html" category="">
       <description>Generate codepeer messages in html format</description>
       <command-help>{help}</command-help>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="2" lines="5">
         <check label="Show informationals" switch="--show-info"
               column="1"
               tip="Show CodePeer informational messages"/>
         <check label="Hide low messages" switch="--hide-low"
               column="1"
               tip="Do not generate messages ranked low"/>

         <field label="Display cpm"
                tip="Select a cpm file to display instead."
                switch="--current"
                separator=" "
                as-file="true"
                file-filter="*.cpm"/>

         <field label="Compare with"
                tip="Compare the current run with an arbitrary cpm file."
                switch="--compare-with"
                separator=" "
                as-file="true"
                file-filter="*.cpm"/>

       </switches>
    </target-model>

    <target-model name="codepeer_bridge" category="">
       <description>Load CodePeer messages</description>
       <iconname>gps-build-all-symbolic</iconname>
       <command-line>
          <arg>cpm-gs-bridge</arg>
          <arg>--log-to</arg>
          <arg>%O</arg>
       </command-line>
    </target-model>

    <target model="generate_scil" category="CodePeer" name="CPL Generate SCIL"
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

    <target model="gprclean" category="CodePeer" name="CPL Remove SCIL"
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

    <target model="html" category="CodePeer"
            name="CPL Generate HTML Report" messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-compile-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-d</arg>
          <arg>--no-analysis</arg>
          <arg>--html</arg>
       </command-line>
    </target>

    <target model="csv" category="CodePeer"
            name="CPL Generate CSV Report" messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-compile-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-d</arg>
          <arg>--csv</arg>
          <arg>--no-analysis</arg>
          <arg>--out</arg>
          <arg>codepeer.csv</arg>
       </command-line>
    </target>

    <target model="codepeer_bridge" category="CodePeer"
            name="CPL CodePeer Bridge" messages_category="CodePeer Bridge">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>cpm-gs-bridge</arg>
          <arg>--log-to</arg>
          <arg>%O</arg>
       </command-line>
    </target>

    <target-model name="codepeer_output_only" category="">
       <description>regenerate report</description>
       <command-help>{help}</command-help>
       <command-line>
          <arg>codepeer</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--gs</arg>
          <arg>--no-analysis</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="3" lines="2">
         <combo label="Analysis level" switch="--level" noswitch="default"
               separator=" " column="1"
               tip="Analysis level for which you want to regenerate a report." >
            <combo-entry label="Last analysis level" value="default" />
            <combo-entry label="0" value="0" />
            <combo-entry label="1" value="1" />
            <combo-entry label="2" value="2" />
            <combo-entry label="3" value="3" />
            <combo-entry label="4" value="4" />
         </combo>

         <check label="Bump baseline" switch="--bump-baseline" column="2"
          tip="make the last run of the selected level the new baseline run."/>

         <field label="Set baseline"
                tip="Select a cpm file as the new baseline."
                switch="--set-baseline"
                separator=" "
                as-file="true"
                file-filter="*.cpm"/>

         <field label="Display cpm"
                tip="Select a cpm file to display instead."
                switch="--current"
                separator=" "
                as-file="true"
                file-filter="*.cpm"/>

         <field label="Compare with"
                tip="Compare the current run with an arbitrary cpm file."
                switch="--compare-with"
                separator=" "
                as-file="true"
                file-filter="*.cpm"/>

         <hidden switch="--dbg-on" separator=" "/>
         <hidden switch="--dbg-off" separator=" "/>
       </switches>
    </target-model>
  </CODEPEER>
"""

xml_codepeer = doc + attributes + tool + body

xmlHead = """<?xml version="1.0"?>
  <CODEPEER>
    <target-model name="codepeer" category="">
       <description>Review code with codepeer</description>
       <command-help>{help}</command-help>
       <command-line>
          <arg>codepeer</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-d</arg>
          <arg>--gs</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="3" lines="6">
         <combo label="Analysis level" switch="--level" noswitch="default"
               separator=" " column="1"
               tip="Set the accuracy and speed of the analysis. Use 0 or 1 for
local and quick analysis, 2 for an intermediate analysis, 3 for a relatively
global analysis (within the memory constraints), and 4 to force a global
analysis. See CodePeer documentation for more details." >
            <combo-entry label="default level" value="default" />
            <combo-entry label="0" value="0" />
            <combo-entry label="1" value="1" />
            <combo-entry label="2" value="2" />
            <combo-entry label="3" value="3" />
            <combo-entry label="4" value="4" />
         </combo>
         <combo label="Messages" switch="--messages" noswitch="default"
               separator=" " column="1"
               tip="Level of verbosity for messages generated by CodePeer" >
            <combo-entry label="Default" value="default" />
            <combo-entry label="Min" value="min" />
            <combo-entry label="Normal" value="normal" />
            <combo-entry label="Max" value="max" />
         </combo>

         <check label="No race conditions analysis" switch="--no-race-conditions"
                column="2" tip="Do not perform race conditions analysis" />
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
         <check label="User-defined GNATcheck rules" switch="--gnatcheck"
                column="2" tip="Launch GNATcheck on the list of rules specified in the Check package of the project file, and the default rules (unless overriden by the next option)" />
         <check label="Skip default GNATcheck rules" switch="--no-default-gnatcheck-rules"
                column="2" tip="Do not launch GNATcheck with the rules enabled by default in CodePeer" />
         <check label="GNAT warnings" switch="--gnat-warnings"
                column="2"
                tip="Launch GNAT front-end and collect its warnings" />
"""

xmlTrailer = """
         <hidden switch="--dbg-on" separator=" "/>
         <hidden switch="--dbg-off" separator=" "/>
       </switches>
    </target-model>

    <target model="codepeer" category="CodePeer" name="CPL Run CodePeer"
            messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-d</arg>
          <arg>--gs</arg>
       </command-line>
    </target>

    <target model="codepeer" category="CodePeer" name="CPL Run CodePeer..."
            messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-d</arg>
          <arg>--gs</arg>
       </command-line>
    </target>

    <target model="codepeer" category="CodePeer"
            name="CPL Run CodePeer File"
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
          <arg>--file</arg>
          <arg>%fp</arg>
       </command-line>
    </target>

    <target model="codepeer_output_only" category="CodePeer"
            name="CPL Regenerate CodePeer Report" messages_category="CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>--gs</arg>
          <arg>--no-analysis</arg>
       </command-line>
    </target>

  </CODEPEER>
"""
