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

import os.path

import GPS
import gps_utils.highlighter
import os_utils

class GNATcovPlugin(object):

    PLUGIN_MENU = '/Tools/GNATcov/'

    # Keep this style name synchronized with Code_Coverage.GNATcov.
    INLINED_DETAILS_NAME = 'GNATcov inlined details'

    PROJECT_SUPPORT_XML = """
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
          <choice>stmt+decision</choice>
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
          <choice>stmt+decision</choice>
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

    BUILD_TARGETS_AND_MODES_XML = """
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
          menu="{menu}">
        <target-type>executable</target-type>
        <in-toolbar>FALSE</in-toolbar>
        <in-menu>TRUE</in-menu>
        <read-only>TRUE</read-only>
        <icon>gps-build-all</icon>
        <launch-mode>MANUALLY</launch-mode>
        <command-line>
          <arg>gnatcov</arg>
          <arg>run</arg>
          <arg>-P%PP</arg>
          <arg>--recursive</arg>
          <arg>%target</arg>
          <arg>-c</arg>
          <arg>%attr(ide_coverage'level_coverage,stmt)</arg>
          <arg>-o</arg>
          <arg>%TT.trace</arg>
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
          <arg>-P%PP</arg>
          <arg>--recursive</arg>
          <arg>%target</arg>
          <arg>--annotate=xcov</arg>
        </command-line>
        <icon>gps-build-all</icon>
        <switches command="%(tool_name)s" columns="1" lines="4">
        </switches>
      </target-model>

      <target model="gnatcov-coverage" category="GNATcov coverage"
              name="Generate GNATcov Main Report"
              menu="{menu}">
        <target-type>executable</target-type>
        <in-toolbar>FALSE</in-toolbar>
        <in-menu>TRUE</in-menu>
        <read-only>TRUE</read-only>
        <icon>gps-build-all</icon>
        <launch-mode>MANUALLY</launch-mode>
        <command-line>
          <arg>gnatcov</arg>
          <arg>coverage</arg>
          <arg>-P%PP</arg>
          <arg>--recursive</arg>
          <arg>%target</arg>
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
       <supported-model filter="--subdirs=">gnatcov-run</supported-model>
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
    """.format(menu=PLUGIN_MENU)

    PREFERENCES_XML = """
      <preference
          name="GNATcov-Inlined-Details-Foreground"
          page="Plugins/GNATcov"
          default="#000000"
          label="Inline details foreground"
          tip="Color to use for the foreground of inlined details"
          type="color" />
      <preference
          name="GNATcov-Inlined-Details-Background"
          page="Plugins/GNATcov"
          default="#E9E9E9"
          label="Inline details background"
          tip="Color to use for the background of inlined details"
          type="color" />
    """

    def __init__(self):
        # Create the GNATcov menu entry before loading targets and so on, so
        # that we master where the entry is inserted.
        GPS.Menu.create(
            self.PLUGIN_MENU + '-',
            ref='Coverage',
            add_before=False)

        # The following are needed to load projets, so do not wait the
        # "gps_started" event.
        GPS.parse_xml(self.PROJECT_SUPPORT_XML)
        GPS.parse_xml(self.PREFERENCES_XML)
        GPS.parse_xml(self.BUILD_TARGETS_AND_MODES_XML)

    def on_gps_started(self, hook):

        GPS.Hook('compilation_finished').add(self.on_compilation_finished)
        GPS.Hook('preferences_changed').add(self.on_preferences_changed)

        self.inline_details_style = GPS.Style(self.INLINED_DETAILS_NAME)

        self.GPS = GPS
        self.on_preferences_changed('', reload=False)

    def reload_gnatcov_data(self):
        """Clean the coverage report and reload it from the files."""
        GPS = self.GPS

        # If needed, switch to GNATcov build mode.
        if GPS.Preference("Coverage-Toolchain").get() != 'Gnatcov':
            GPS.Preference("Coverage-Toolchain").set('Gnatcov')

        GPS.execute_action("/Tools/Coverage/Clear coverage from memory")
        GPS.execute_action("/Tools/Coverage/Load data for all projects")

    def on_compilation_finished(self, hook, category,
        target_name="", mode_name="", status=""):
        """Called whenever a compilation ends."""

        # If compilation failed, do nothing.
        if status:
            return

        if target_name in ["Generate GNATcov Main Report"]:
            self.reload_gnatcov_data()

    def on_preferences_changed(self, hook, reload=True):
        """Update various plugin elements that rely on preferences."""
        GPS = self.GPS

        self.inline_details_style.set_background(
            GPS.Preference('GNATcov-Inlined-Details-Background').get())
        self.inline_details_style.set_foreground(
            GPS.Preference('GNATcov-Inlined-Details-Foreground').get())

def setup_plugin(hook):
    # To make debugging easier, keep a reference to the plugin in the module
    # namespace.
    global plugin

    # Do nothing if GNATcoverage is not available.
    if not os_utils.locate_exec_on_path('gnatcov'):
        return

    plugin = GNATcovPlugin()
    GPS.Hook('gps_started').add(plugin.on_gps_started)

setup_plugin(None)
