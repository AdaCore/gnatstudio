"""This file provides support for using the CodePeer tool
   Note that this is a work in progress.

   CodePeer is a static analysis tool for Ada code.
   This tool allows the user to perform an automatic code review of
   a project and integrates its output into GPS.
   See menu Tools->CodePeer.
"""


############################################################################
## No user customization below this line
############################################################################

import GPS, os_utils, os.path

# Check for GNAT toolchain: codepeer, gps_codepeer_bridge

codepeer = os_utils.locate_exec_on_path("codepeer")

if codepeer != "":
  example_root=os.path.dirname (os.path.dirname(codepeer)).replace('\\', '/')+\
    '/share/examples/codepeer'
  GPS.parse_xml ("""
    <doc_path>share/doc/codepeer</doc_path>

    <documentation_file>
      <name>codepeer_ug.html</name>
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

    <action name="codepeer_example_false_tests" category=""
            show-command="false" output="none">
      <shell>Project.load """ + '"' + example_root + \
             """/false_tests/example.gpr"</shell>
      <shell>Editor.edit "false_tests.adb"</shell>
      <shell>Editor.edit "README.txt"</shell>
    </action>

    <submenu before="About">
      <title>/Help/CodePeer/Examples</title>
      <menu action="codepeer_example_false_tests">
        <title>False Tests</title>
      </menu>
    </submenu>

    <action name="codepeer_example_uninitialized" category=""
            show-command="false" output="none">
      <shell>Project.load """ + '"' + example_root + \
             """/uninitialized/uninitialized.gpr"</shell>
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

    <action name="codepeer_example_contacts" category=""
            show-command="false" output="none">
      <shell>Project.load """ + '"' + example_root + \
             """/contacts/contacts.gpr"</shell>
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
      <shell>Project.load """ + '"' + example_root + \
             """/cruise/cruise.gpr"</shell>
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

    <action name="codepeer_example_configs" category=""
            show-command="false" output="none">
      <shell>Project.load """ + '"' + example_root + \
             """/configs/configs.gpr"</shell>
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

    <action name="codepeer_example_radar" category=""
            show-command="false" output="none">
      <shell>Project.load """ + '"' + example_root + \
             """/radar/radar.gpr"</shell>
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

    <action name="codepeer_example_conductor" category=""
            show-command="false" output="none">
      <shell>Project.load """ + '"' + example_root + \
             """/conductor/conductor.gpr"</shell>
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

    <target-model name="generate_scil" category="">
       <description>Generate SCIL files for CodePeer</description>
       <command-line>
          <arg>gnatmake</arg>
          <arg>-d</arg>
          <arg>%eL</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <switches command="%(tool_name)s" columns="1" lines="1">
         <title column="1" line="1" >Compilation</title>
         <check label="Progress bar" switch="-d" column="1"
                tip="Display a progress bar with information about how many files are left to be compiled" />
         <check label="Ignore representation clauses" switch="-gnatI" column="1"
                tip="Ignore all representation clauses, useful for generating SCIL for another architecture" />
         <check label="GNAT warnings" switch="-gnatwna" column="1"
                tip="Enable GNAT warnings during SCIL generation" />
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
        <substitute src="%builder" dest="gnatmake"/>
      </substitutions>
      <extra-args>
        <arg>-margs</arg>
        <arg>--subdirs=%subdir</arg>
        <arg>-j1</arg>
        <arg>-k</arg>
        <arg>-c</arg>
        <arg>-gnatcC</arg>
      </extra-args>
    </builder-mode>

    <target-model name="codepeer" category="">
       <description>Review code with codepeer</description>
       <command-line>
          <arg>codepeer</arg>
          <arg>-all</arg>
          <arg>-background</arg>
          <arg>-dbg-on</arg>
          <arg>ide_progress_bar</arg>
          <arg>-lib</arg>
          <arg>%Pb.library</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <switches command="%(tool_name)s" columns="3" lines="5">
         <check label="Global analysis" switch="-global" column="1"
                tip="Do not split analysis in partitions" />
         <check label="Baseline run" switch="-baseline" column="2"
          tip="this run is a baseline run and prior run becomes default cutoff"
         />
         <spin label="Cutoff" switch="-cutoff" min="1" max="100000"
               default="1" separator=" " column="3"
               tip="This run should use id as the cutoff for the base column" />
         <check label="Re-partition" switch="-repartition"
                column="1" tip="Recompute partition libraries instead of reusing partitions from previous run (ignored with -global)" />
         <spin label="Partition limit" switch="-dbg-partition-limit"
               min="50" max="100000" default="15000"
               separator=" " column="2"
               tip="Size of each partition to analyze (ignored with -global)" />
         <spin label="Multiprocessing" switch="-jobs" min="1" max="100"
               default="1" separator=" " column="3"
               tip="Use N processes to carry out the analysis." />
         <check label="No race condition" switch="-no-race-conditions"
                column="1" tip="Do not perform race conditions analysis" />
         <combo label="Messages" switch="-messages" noswitch="normal"
               separator=" " column="2" column-span="2"
               tip="Level of verbosity for messages generated by CodePeer" >
            <combo-entry label="Normal" value="normal" />
            <combo-entry label="Min" value="min" />
            <combo-entry label="Max" value="max" />
         </combo>
         <spin label="Subp memory size" switch="-method-memory-size"
               min="1" max="4096" default="540"
               separator=" " column="1"
               tip="Max amount of memory (in megs) to use for analyzing a subprogram" />
         <spin label="Subp timeout" switch="-method-timeout"
               min="1" max="10000" default="600"
               separator=" " column="2"
               tip="Max time (in seconds) spent analyzing a subprogram before giving up" />
       </switches>
    </target-model>

    <target model="generate_scil" category="CodePeer" name="Generate SCIL">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatmake</arg>
          <arg>-d</arg>
          <arg>%eL</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
          <arg>-gnatcC</arg>
       </command-line>
    </target>

    <target model="gprclean" category="CodePeer" name="Remove SCIL">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <icon>gps-clean</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>%gprclean</arg>
          <arg>-q</arg>
          <arg>-r</arg>
          <arg>%eL</arg>
          <arg>-P%PP</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <target model="codepeer" category="CodePeer" name="Run CodePeer">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-all</arg>
          <arg>-background</arg>
          <arg>-repartition</arg>
          <arg>-dbg-on</arg>
          <arg>ide_progress_bar</arg>
          <arg>-lib</arg>
          <arg>%Pb.library</arg>
       </command-line>
    </target>

    <target model="codepeer" category="CodePeer" name="Run CodePeer Quickly">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-all</arg>
          <arg>-background</arg>
          <arg>-repartition</arg>
          <arg>-no-race-conditions</arg>
          <arg>-dbg-on</arg>
          <arg>ide_progress_bar</arg>
          <arg>-dbg-partition-limit</arg>
          <arg>1000</arg>
          <arg>-method-timeout</arg>
          <arg>30</arg>
          <arg>-no-html-output</arg>
          <arg>-lib</arg>
          <arg>%Pb.library</arg>
       </command-line>
    </target>

    <target model="codepeer" category="CodePeer"
            name="Regenerate CodePeer Report">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>codepeer</arg>
          <arg>-all</arg>
          <arg>-background</arg>
          <arg>-output-only</arg>
          <arg>-lib</arg>
          <arg>%Pb.library</arg>
       </command-line>
    </target>
""")
