"""
This file provides default build modes and targets (gnatmake and gprbuild) for
GPS
"""

XML = r"""<?xml version="1.0" ?>
<GPS>
 <builder-mode name="default">
  <description>Build with default switches defined in the project</description>
 </builder-mode>

 <builder-mode name="debug">
  <description>Build with debug information</description>
  <subdir>debug</subdir>
  <supported-model>builder</supported-model>
  <supported-model>gnatmake</supported-model>
  <supported-model>gprbuild</supported-model>
  <supported-model filter="--subdirs=">gprclean</supported-model>
  <extra-args>
     <arg>--subdirs=%subdir</arg>
     <arg>-cargs</arg>
     <arg>-g</arg>
     <arg>-O0</arg>
  </extra-args>
 </builder-mode>

 <builder-mode name="checks">
  <description>Build with full checking enabled</description>
  <subdir>check</subdir>
  <supported-model>builder</supported-model>
  <supported-model>gnatmake</supported-model>
  <supported-model>gprbuild</supported-model>
  <supported-model filter="--subdirs=">gprclean</supported-model>
  <substitutions>
    <substitute src="%builder" dest="%gprbuild"/>
    <substitute src="%gnatmake" dest="%gprbuild"/>
  </substitutions>
  <extra-args>
     <arg>--subdirs=%subdir</arg>
     <arg>-cargs:Ada</arg>
     <arg>-g</arg>
     <arg>-O0</arg>
     <arg>-gnato</arg>
     <arg>-fstack-check</arg>
     <arg>-gnatVa</arg>
     <arg>-cargs:C</arg>
     <arg>-g</arg>
     <arg>-O0</arg>
     <arg>-fstack-check</arg>
     <arg>-cargs:C++</arg>
     <arg>-g</arg>
     <arg>-O0</arg>
     <arg>-fstack-check</arg>
  </extra-args>
 </builder-mode>

 <builder-mode name="optimize">
  <description>Build for production with full optimization</description>
  <subdir>opt</subdir>
  <supported-model>builder</supported-model>
  <supported-model>gnatmake</supported-model>
  <supported-model>gprbuild</supported-model>
  <supported-model filter="--subdirs=">gprclean</supported-model>
  <substitutions>
    <substitute src="%builder" dest="%gprbuild"/>
    <substitute src="%gnatmake" dest="%gprbuild"/>
  </substitutions>
  <extra-args>
     <arg>--subdirs=%subdir</arg>
     <arg>-cargs:Ada</arg>
     <arg>-O2</arg>
     <arg>-gnatn</arg>
     <arg>-cargs:C</arg>
     <arg>-O2</arg>
     <arg>-cargs:C++</arg>
     <arg>-O2</arg>
  </extra-args>
 </builder-mode>

 <builder-mode name="gcov">
  <description>Build with gcov support</description>
  <subdir>gcov</subdir>
  <supported-model>builder</supported-model>
  <supported-model>gnatmake</supported-model>
  <supported-model>gprbuild</supported-model>
  <supported-model filter="--subdirs=">gprclean</supported-model>
  <extra-args>
     <arg>--subdirs=%subdir</arg>
     <arg>-cargs</arg>
     <arg>-g</arg>
     <arg>-fprofile-arcs</arg>
     <arg>-ftest-coverage</arg>
     <arg>-largs</arg>
     <arg>--coverage</arg>
  </extra-args>
 </builder-mode>

 <builder-mode name="gprof">
  <description>Build with gprof support</description>
  <subdir>gprof</subdir>
  <supported-model>builder</supported-model>
  <supported-model>gnatmake</supported-model>
  <supported-model>gprbuild</supported-model>
  <supported-model filter="--subdirs=">gprclean</supported-model>
  <extra-args>
     <arg>--subdirs=%subdir</arg>
     <arg>-cargs</arg>
     <arg>-g</arg>
     <arg>-pg</arg>
     <arg>-largs</arg>
     <arg>-pg</arg>
  </extra-args>
 </builder-mode>

 <target-model name="builder" category="">
   <description>Generic GNAT builder</description>
   <command-line>
      <arg>%builder</arg>
      <arg>-d</arg>
      <arg>%eL</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
   </command-line>
   <icon>gps-build-all</icon>
   <switches command="%(tool_name)s" columns="2" lines="2">
     <title column="1" line="1" >Dependencies</title>
     <title column="2" line="1" >Compilation</title>
     <title column="2" line="2" >Project</title>
     <check label="Recompile if switches changed" switch="-s"
            tip="Recompile if compiler switches have changed
            since last compilation" />
     <check label="Keep going" switch="-k"
            tip="Continue as much as possible after a compilation error" />
     <spin label="Multiprocessing" switch="-j" min="0" max="100" default="1"
           column="2"
           tip="Use N processes to carry out the compilations.
           On a multiprocessor machine compilations will occur in parallel" />
     <check label="Progress bar" switch="-d" column="2"
            tip="Display a progress bar with information about how many files
            are left to be compiled" />
     <check label="Compile only" switch="-c" column="2"
            tip="Perform only compilation, no bind/link" />
     <check label="Quiet mode" switch="-q" column="2"
            tip="Be quiet/terse in output messages" />
     <check label="Create object dirs" switch="-p" line="2" column="2"
            tip="Create missing object and library directories" />
     <spin label="Project verbosity" switch="-vP" min="0" max="2" default="1"
           line="2" column="2"
           tip="Specify verbosity when parsing project files" />
   </switches>
</target-model>

<!-- This is an XML model for gprbuild -->
<target-model name="gprbuild" category="">
   <description>Build with gprbuild</description>
   <command-line>
      <arg>%gprbuild</arg>
      <arg>-d</arg>
      <arg>%eL</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
   </command-line>
   <icon>gps-build-all</icon>
   <switches command="%(tool_name)s" columns="2" lines="2">
     <title column="1" line="1" >Dependencies</title>
     <title column="2" line="1" >Compilation</title>
     <title column="2" line="2" >Project</title>
     <check label="Recompile if switches changed" switch="-s"
            tip="Recompile if compiler switches have changed since last
            compilation" />
     <check label="Keep going" switch="-k"
            tip="Continue as much as possible after a compilation error" />
     <spin label="Multiprocessing" switch="-j" min="0" max="100" default="1"
           column="2"
           tip="Use N processes to carry out the compilations.
           On a multiprocessor machine compilations will occur in parallel" />
     <check label="Progress bar" switch="-d" column="2"
            tip="Display a progress bar with information about how many files
            are left to be compiled" />
     <check label="Compile only" switch="-c" column="2"
            tip="Perform only compilation, no bind/link" />
     <check label="Quiet mode" switch="-q" column="2"
            tip="Be quiet/terse in output messages" />
     <check label="Create object dirs" switch="-p" line="2" column="2"
            tip="Create missing object and library directories" />
     <spin label="Project verbosity" switch="-vP" min="0" max="2" default="1"
           line="2" column="2"
           tip="Specify verbosity when parsing project files" />
   </switches>
</target-model>

<!-- This is an XML model for gnatmake -->
<target-model name="gnatmake" category="">
   <description>Build with gnatmake</description>
   <command-line>
      <arg>%gnatmake</arg>
      <arg>-d</arg>
      <arg>%eL</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
   </command-line>
   <icon>gps-build-all</icon>
   <switches command="%(tool_name)s" columns="2" lines="2">
     <title column="1" line="1" >Dependencies</title>
     <title column="1" line="2" >Checks</title>
     <title column="2" line="1" >Compilation</title>
     <title column="2" line="2" >Project</title>
     <check label="Recompile if switches changed" switch="-s"
            tip="Recompile if compiler switches have
            changed since last compilation" />
     <check label="Minimal recompilation" switch="-m"
            tip="Specifies that the minimum necessary amount of recompilation
            be performed. In this mode, gnatmake ignores time
            stamp differences when the only modification to a source
            file consist in adding or removing comments,
            empty lines, spaces or tabs" />
     <check label="Keep going" switch="-k"
            tip="Continue as much as possible after a compilation error" />
     <spin label="Multiprocessing" switch="-j" min="0" max="100" default="1"
           column="2"
           tip="Use N processes to carry out the compilations. On a
           multiprocessor machine compilations will occur in parallel" />
     <check label="Progress bar" switch="-d" column="2"
            tip="Display a progress bar with information about how many
            files are left to be compiled" />
     <check label="Compile only" switch="-c" column="2"
            tip="Perform only compilation, no bind/link" />
     <check label="Quiet mode" switch="-q" column="2"
            tip="Be quiet/terse in output messages" />
     <check label="Debug information" switch="-g" column="2"
            tip="Add debugging information. This forces the corresponding
            switch for the compiler, binder and linker" />

     <check label="Syntax check" switch="-gnats" line="2"
            tip="Perform syntax check, no compilation occurs" />
     <check label="Semantic check" switch="-gnatc" line="2"
            tip="Perform syntax and semantic check only, no
            compilation occurs" />
     <check label="Create object dirs" switch="-p" line="2" column="2"
            tip="Create missing object and library directories" />
     <spin label="Project verbosity" switch="-vP" min="0" max="2" default="1"
           line="2" column="2"
           tip="Specify verbosity when parsing project files" />
   </switches>
</target-model>

<!-- This is an XML model for gnatclean/gprclean -->
<target-model name="gprclean" category="">
   <description>Clean compilation artefacts with gnatclean/gprclean
   </description>
   <command-line>
      <arg>%gprclean</arg>
      <arg>%eL</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
   </command-line>
   <icon>gps-clean</icon>
   <switches command="%(tool_name)s" columns="1">
     <check label="Only delete compiler generated files" switch="-c"
            tip="Remove only the files generated by the compiler,
            not other files" />
     <check label="Force deletion" switch="-f"
            tip="Force deletions of unwritable files" />
     <check label="Clean recursively" switch="-r"
            tip="Clean all projects recursively" />
   </switches>
</target-model>

<!-- This is an XML model for launching executables -->
<target-model name="execute" category="">
   <description>Run an executable</description>
   <command-line>
      <arg>%E</arg>
   </command-line>
   <server>Execution_Server</server>
   <is-run>TRUE</is-run>
   <icon>gtk-media-play</icon>
   <switches command="%(tool_name)s" columns="1">
     <check label="Run in executables directory" switch="[exec_dir]"
     before="true" />
     <check label="Run in an external terminal"
    switch="%external" before="true" />
   </switches>
</target-model>

<!-- This is a minimal XML model, used for launching executables -->
<target-model name="custom" category="">
   <description>Launch a custom build command</description>
   <icon>gps-custom-build</icon>
   <uses-shell>TRUE</uses-shell>
   <switches command="">
   </switches>
</target-model>

<!-- This is a minimal XML model, used for launching python scripts -->
<target-model name="python" category="">
   <description>Launch a custom build command</description>
   <icon>gps-custom-build</icon>
   <uses-python>TRUE</uses-python>
   <switches command="">
   </switches>
</target-model>

<!-- NOTE: the name of this command must be kept in sync with the constant in
     Builder_Facility_Module.Scripts.  -->
<target model="gnatmake" category="_File_" name="Check _Syntax">
    <icon>gps-syntax-check</icon>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <server>Tools_Server</server>
    <command-line>
       <arg>%gnatmake</arg>
       <arg>-q</arg>
       <arg>-f</arg>
       <arg>-c</arg>
       <arg>-gnats</arg>
       <arg>-u</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
       <arg>%fp</arg>
    </command-line>
</target>

<!-- NOTE: the name of this command must be kept in sync with the constant in
     Builder_Facility_Module.Scripts.  -->
<target model="gnatmake" category="_File_" name="Check S_emantic">
    <in-toolbar>TRUE</in-toolbar>
    <icon>gps-semantic-check</icon>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <server>Tools_Server</server>
    <command-line>
       <arg>%gnatmake</arg>
       <arg>-q</arg>
       <arg>-c</arg>
       <arg>-f</arg>
       <arg>-gnatc</arg>
       <arg>-u</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
       <arg>%fp</arg>
    </command-line>
</target>

<target model="gnatmake" category="_File_" name="U_pdate file XRef">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <icon>gps-semantic-check</icon>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <server>Tools_Server</server>
    <always-clear-locations>FALSE</always-clear-locations>
    <command-line>
       <arg>%gnatmake</arg>
       <arg>-q</arg>
       <arg>-c</arg>
       <arg>-gnatc</arg>
       <arg>-u</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
       <arg>%fp</arg>
    </command-line>
    <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         end_of_build
    </output-parsers>
</target>

<target model="gnatmake" category="_File_"
name="U_pdate file XRef in background">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <icon>gps-semantic-check</icon>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <server>Tools_Server</server>
    <always-clear-locations>FALSE</always-clear-locations>
    <command-line>
       <arg>%gnatmake</arg>
       <arg>-q</arg>
       <arg>-c</arg>
       <arg>-gnatc</arg>
       <arg>-u</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
       <arg>%fp</arg>
    </command-line>
    <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         end_of_build
    </output-parsers>
</target>


<!-- This is a target to compile the current file using the builder model
     NOTE: the name of this command must be kept in sync with the constant in
     Builder_Facility_Module.Scripts.  -->
<target model="builder" category="_File_" name="_Compile File">
    <in-toolbar>TRUE</in-toolbar>
    <icon>gps-compile</icon>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
       <arg>%builder</arg>
       <arg>-ws</arg>
       <arg>-c</arg>
       <arg>-f</arg>
       <arg>-u</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
       <arg>%fp</arg>
    </command-line>
</target>

<target model="builder" category="_Project" name="Build Main">
    <in-toolbar>TRUE</in-toolbar>
    <in-contextual-menus-for-projects>TRUE</in-contextual-menus-for-projects>
    <icon>gps-build-main</icon>
    <launch-mode>MANUALLY</launch-mode>
    <read-only>TRUE</read-only>
    <target-type>main</target-type>
    <command-line>
       <arg>%builder</arg>
       <arg>-d</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
       <arg>%T</arg>
    </command-line>
</target>

<!-- Targets to compile all project files using the builder model  -->
<target model="builder" category="_Project" name="_Build All">
    <in-toolbar>TRUE</in-toolbar>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
       <arg>%builder</arg>
       <arg>-d</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
    </command-line>
</target>
<target model="builder" category="_Project" name="_Compile All Sources">
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
       <arg>%builder</arg>
       <arg>-c</arg>
       <arg>-U</arg>
       <arg>-d</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
    </command-line>
</target>

<target model="builder" category="_Project" name="Build &lt;current file&gt;">
    <icon>gps-build-main</icon>
    <launch-mode>MANUALLY</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
       <arg>%builder</arg>
       <arg>-d</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
       <arg>%fp</arg>
    </command-line>
</target>

<!-- Targets to clear the current project using the gprclean model  -->
<target model="gprclean" category="C_lean" name="Clean _All">
    <in-toolbar>TRUE</in-toolbar>
    <icon>gps-clean</icon>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
       <arg>%gprclean</arg>
       <arg>-r</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
    </command-line>
</target>
<target model="gprclean" category="C_lean" name="Clean _Root">
    <in-toolbar>FALSE</in-toolbar>
    <icon>gps-clean</icon>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
       <arg>%gprclean</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
    </command-line>
</target>
<target model="gprclean" category="C_lean" name="Clean _Project">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <in-contextual-menus-for-projects>TRUE</in-contextual-menus-for-projects>
    <icon>gps-clean</icon>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
       <arg>%gprclean</arg>
       <arg>%eL</arg>
       <arg>-P%pp</arg>
       <arg>%X</arg>
    </command-line>
</target>

<!-- Targets to launch runs  -->
<target model="execute" category="_Run" name="Run _Main">
    <in-toolbar>TRUE</in-toolbar>
    <in-contextual-menus-for-projects>TRUE</in-contextual-menus-for-projects>
    <icon>gtk-media-play</icon>
    <launch-mode>MANUALLY</launch-mode>
    <read-only>TRUE</read-only>
    <target-type>executable</target-type>
    <command-line>
       <arg>%E</arg>
    </command-line>
</target>

<target model="execute" category="_Run" name="Custom...">
    <in-toolbar>FALSE</in-toolbar>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line />
</target>

<!-- This is an empty target using the Custom model  -->
<target model="custom" category="_Project" name="Custom _Build...">
    <in-toolbar>FALSE</in-toolbar>
    <icon>gps-custom-build</icon>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line />
 </target>
</GPS>
"""

import GPS
GPS.parse_xml(XML)
