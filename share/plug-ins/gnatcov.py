""" This plug-in adds support for GNATcoverage.

This plug-in provides the following:
   * A new Build Mode "gnatcov"
   * Several new project attributes which GPS will
     use to drive various tools in the context of
     GNATcoverage
   * Build targets to launch runs and analyses
   * Menus corresponding to these build targets.

The Build Mode "gnatcov" is listed in the Build Mode
combo, in the main toolbar. Objects generated under
this build mode are generated in a subdirectory "gnatcov"
in all object and executable directories specified by
the project hierarchy.

The following Project Properties are added, which are
available in the "GNATcov" section of the Project
Properties editor, and which map to attributes in a
package "IDE_Coverage" in the project files.

  * Gnatcov_Mode_Switches: switches that GPS will pass
    to the command line used to build while the "gnatcov"
    Build Mode is selected

  * Level_Run: the coverage level to pass to the
    "gnatcov run" command

  * Switches_Run: additional switches to pass to
    the "gnatcov run" command

  * Level_Coverage: the coverage level to pass to
    the "gnatcov coverage" command

  * Switches_Coverage: additional switches to pass
    to the "gnatcov coverage" command

This plugin defines two new build targets, to launch
"gnatcov run" and "gnatcov coverage", automatically
generated for every executable defined in the project
hierarchy, along with menus, under the menu
   Tools->GNATcoverage.

In addition, this plugin automatically loads or refreshes
the Coverage Report in GPS after every call to the
"gnatcov coverage" build target.

With this plugin, the steps to follow for a typical
GNATcoverage session would be:
  1 - switch to the "gnatcov" Build Mode in the toolbar
  2 - build the executable using the standard mechanism
  3 - launch a first run using the menu
      Tools->GNATcoverage->Run under gnatcov
  4 - launch a first analysis using the menu
      Tools->GNATcoverage->Coverage with gnatcov
  5 - edit the code or the test driver, then rerun
      steps 2, 3, 4

Note: this plug-in activates only when the command-line tool
"gnatcov" is found on the PATH.
"""


###########################################################################
## No user customization below this line
###########################################################################

import GPS, os.path, os_utils;

#
# Project support
#

project_support_xml = """

  <project_attribute
    name="Gnatcov_Mode_Switches"
    label="Switches in 'gnatcov' mode"
    package="IDE_Coverage"
    editor_page="GNATcov"
    editor_section="Build"
    hide_in="wizard library_wizard"
    description="Extra build switches to pass to the builder when in 'gnatcov' mode.">
    <string />
  </project_attribute>

  <project_attribute
    name="Level_Run"
    label="Coverage Level"
    package="IDE_Coverage"
    editor_page="GNATcov"
    editor_section="Run"
    hide_in="wizard library_wizard"
    description="The coverage level to pass to gnatcov run.">

      <choice>branch</choice>
      <choice>insn</choice>
      <choice default="true">stmt</choice>
      <choice>stmt+decisison</choice>
      <choice>stmt+mcdc</choice>

  </project_attribute>

  <project_attribute
    name="Switches_Run"
    label="Extra switches"
    package="IDE_Coverage"
    editor_page="GNATcov"
    editor_section="Run"
    hide_in="wizard library_wizard"
    description="Extra build switches to pass to gnatcov run">
    <string />
  </project_attribute>

  <project_attribute
    name="Level_Coverage"
    label="Coverage Level"
    package="IDE_Coverage"
    editor_page="GNATcov"
    editor_section="Coverage"
    hide_in="wizard library_wizard"
    description="The coverage level to pass to gnatcov coverage.">

      <choice>branch</choice>
      <choice>insn</choice>
      <choice default="true">stmt</choice>
      <choice>stmt+decisison</choice>
      <choice>stmt+mcdc</choice>

  </project_attribute>

  <project_attribute
    name="Switches_Coverage"
    label="Extra switches"
    package="IDE_Coverage"
    editor_page="GNATcov"
    editor_section="Coverage"
    hide_in="wizard library_wizard"
    description="Extra build switches to pass to gnatcov coverage">
    <string />
  </project_attribute>
"""

#
# Build targets and modes
#

xml = """

  <!--  Program execution under instrumented execution environment  -->

  <target-model name="gnatcov-run" category="">
    <description>Run under GNATcov for code coverage</description>
    <command-line>
      <arg>gnatcov</arg>
      <arg>run</arg>
    </command-line>
    <icon>gps-build-all</icon>
    <switches command="%(tool_name)s" columns="2" lines="2">
    </switches>
  </target-model>

  <target model="gnatcov-run" category="GNATcov run"
      name="Run under GNATcov"
      @MENU@>
    <target-type>executable</target-type>
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>TRUE</in-menu>
    <read-only>TRUE</read-only>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY</launch-mode>
    <command-line>
      <arg>gnatcov</arg>
      <arg>run</arg>
      <arg>-c</arg>
      <arg>%attr(ide_coverage'level_coverage,stmt)</arg>
      <arg>-o</arg>
      <arg>%TT.trace</arg>
      <arg>%target</arg>
      <arg>%E</arg>
      <arg>%attr(ide_coverage'switches_run)</arg>
    </command-line>
  </target>

  <!--  Coverage report generation  -->

  <target-model name="gnatcov-coverage" category="">
    <description>Code coverage with GNATcov</description>
    <command-line>
      <arg>gnatcov</arg>
      <arg>coverage</arg>
      <arg>--annotate=xcov</arg>
    </command-line>
    <icon>gps-build-all</icon>
    <switches command="%(tool_name)s" columns="1" lines="4">
    </switches>
  </target-model>

  <target model="gnatcov-coverage" category="GNATcov coverage"
          name="Generate GNATcov Main Report"
          @MENU@>
    <target-type>executable</target-type>
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>TRUE</in-menu>
    <read-only>TRUE</read-only>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY</launch-mode>
    <command-line>
      <arg>gnatcov</arg>
      <arg>coverage</arg>
      <arg>-P</arg>
      <arg>%PP</arg>
      <arg>-c</arg>
      <arg>%attr(ide_coverage'level_coverage,stmt)</arg>
      <arg>--annotate=xcov+</arg>
      <arg>--output-dir=%O</arg>
      <arg>-T</arg>
      <arg>%TT.trace</arg>
      <arg>%attr(ide_coverage'switches_coverage)</arg>
    </command-line>
  </target>

  <builder-mode name="gnatcov">
   <description>Build with GNATcoverage information</description>
   <subdir>gnatcov</subdir>
   <supported-model>builder</supported-model>
   <supported-model>gnatmake</supported-model>
   <supported-model>gprbuild</supported-model>
   <supported-model filter="--subdirs=">gnatcov-coverage</supported-model>
   <supported-model filter="--subdirs=">gprclean</supported-model>
   <extra-args>
      <arg>%attr(ide_coverage'gnatcov_mode_switches)</arg>
      <arg>--subdirs=%subdir</arg>
      <arg>-cargs</arg>
      <arg>-g</arg>
      <arg>-fdump-scos</arg>
      <arg>-fpreserve-control-flow</arg>
   </extra-args>
  </builder-mode>


"""

def reload_gnatcov_data():
    """ Clean the coverage report and reload it from the files.
    """
    # Switch to gcov mode
    if GPS.Preference("Coverage-Toolchain").get() != 'Gnatcov':
        GPS.Preference("Coverage-Toolchain").set('Gnatcov')

    GPS.execute_action("/Tools/Coverage/Clear coverage from memory")
    GPS.execute_action("/Tools/Coverage/Load data for all projects")

def on_gps_started (hook_name):
    """ Called once, when GPS is starting.
    """
    global gnatcov_menu_separator

    if os_utils.locate_exec_on_path ("gnatcov") != "":
        GPS.Hook("compilation_finished").add(on_compilation_finished)

        menu = "/Tools/GNATcov/"
        ref  = "Coverage"
        gnatcov_menu = GPS.Menu.create(menu + '-', ref=ref, add_before=False)

        GPS.parse_xml(xml.replace("@MENU@", 'menu="%s"' % menu))

def on_compilation_finished(hook, category,
    target_name="", mode_name="", status=""):
    """ Called whenever a compilation ends.
    """

    if status:
        return

    if target_name in ["Generate GNATcov Main Report"]:
        reload_gnatcov_data()

GPS.parse_xml (project_support_xml)

GPS.Hook ("gps_started").add (on_gps_started)
