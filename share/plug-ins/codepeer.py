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

import GPS, os.path, os_utils

# Check for GNAT toolchain: codepeer, gps_codepeer_bridge

if os_utils.locate_exec_on_path("codepeer") != "" \
  and os_utils.locate_exec_on_path("gps_codepeer_bridge") != "":
  GPS.parse_xml ("""
    <builder-mode name="codepeer">
      <description>Build SCIL for code review</description>
      <subdir>codepeer</subdir>
      <supported-model>builder</supported-model>
      <supported-model>gnatmake</supported-model>
      <supported-model>gprbuild</supported-model>
      <supported-model filter="--subdirs=">gprclean</supported-model>
      <substitutions>
        <substitute src="%builder" dest="%gnatmake"/>
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
          <arg>-global</arg>
          <arg>-background</arg>
          <arg>-dbg-on</arg>
          <arg>ide_progress_bar</arg>
          <arg>-lib</arg>
          <arg>%Pl.library</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <switches command="%(tool_name)s" columns="1" lines="2">
         <check label="Global analysis" switch="-global"
                tip="Do not split analysis in partitions" />
         <spin label="Multiprocessing" switch="-jobs" min="1" max="100"
               default="1"
               tip="Use N processes to carry out the analysis." />
       </switches>
    </target-model>

    <target model="builder" category="CodePeer" name="Generate SCIL">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>%builder</arg>
          <arg>-d</arg>
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
          <arg>-global</arg>
          <arg>-background</arg>
          <arg>-dbg-on</arg>
          <arg>ide_progress_bar</arg>
          <arg>-lib</arg>
          <arg>%Pl.library</arg>
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
          <arg>-global</arg>
          <arg>-background</arg>
          <arg>-output-only</arg>
          <arg>-lib</arg>
          <arg>%Pl.library</arg>
       </command-line>
    </target>
""")
