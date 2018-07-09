""" This plugin adds support for GNATcoverage.

This plugin provides the following:
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

All these steps can be executed at once via the 'Run GNATcov'
button, which is added to the main toolbar when the plugin
is enabled.

Note: this plugin activates only when the command-line tool
"gnatcov" is found on the PATH.
"""

###########################################################################
# No user customization below this line
###########################################################################

import os.path

import GPS
from extensions.private.xml import X
import os_utils
import workflows.promises as promises
import workflows


def list_to_xml(items):
    return '\n'.join(str(i) for i in items)


gnatcov_path = os_utils.locate_exec_on_path('gnatcov')
gnatcov_install_dir = (
    os.path.join(os.path.dirname(gnatcov_path), '..')
    if gnatcov_path else
    None
)


class GNATcovPlugin(object):

    PLUGIN_MENU = '/Analyze/Coverage/GNATcov/'

    # Keep this style name synchronized with Code_Coverage.GNATcov.

    PROJECT_ATTRIBUTES = [
        X(
            'project_attribute',
            package='IDE_Coverage',
            name='Gnatcov_Mode_Switches',

            label="Switches in 'gnatcov' mode",
            description=("Extra build switches to pass to the builder when in"
                         " 'gnatcov' mode."),

            editor_page='GNATcov',
            editor_section='Build',
            hide_in='wizard library_wizard',
        ).children(X('string')),

        X(
            'project_attribute',
            name='Level_Run',
            label='Coverage Level',
            package='IDE_Coverage',
            editor_page='GNATcov',
            editor_section='Run',
            hide_in='wizard library_wizard',
            description='The coverage level to pass to gnatcov run.',
        ).children(
            X('choice').children('branch'),
            X('choice').children('insn'),
            X('choice', default='true').children('stmt'),
            X('choice').children('stmt+decision'),
            X('choice').children('stmt+mcdc'),
        ),

        X(
            'project_attribute',
            name='Switches_Run',
            label='Extra switches',
            package='IDE_Coverage',
            editor_page='GNATcov',
            editor_section='Run',
            hide_in='wizard library_wizard',
            description='Extra build switches to pass to gnatcov run.',
        ).children(X('string')),

        X(
            'project_attribute',
            name='Level_Coverage',
            label='Coverage Level',
            package='IDE_Coverage',
            editor_page='GNATcov',
            editor_section='Coverage',
            hide_in='wizard library_wizard',
            description='The coverage level to pass to gnatcov coverage.',
        ).children(
            X('choice').children('branch'),
            X('choice').children('insn'),
            X('choice', default='true').children('stmt'),
            X('choice').children('stmt+decision'),
            X('choice').children('stmt+mcdc'),
        ),

        X(
            'project_attribute',
            name='Switches_Coverage',
            label='Extra switches',
            package='IDE_Coverage',
            editor_page='GNATcov',
            editor_section='Coverage',
            hide_in='wizard library_wizard',
            description='Extra build switches to pass to gnatcov coverage.',
        ).children(X('string')),
    ]

    BUILD_MODES = [
        X('builder-mode', name='gnatcov').children(
            X('description').children('Build with GNATcoverage information'),
            X('subdir').children('gnatcov'),
            X('supported-model').children('builder'),
            X('supported-model').children('gnatmake'),
            X('supported-model').children('gprbuild'),
            X('supported-model', filter='--subdirs=').children('gnatcov-run'),
            X('supported-model', filter='--subdirs=').children(
                'gnatcov-coverage'),
            X('supported-model', filter='--subdirs=').children('gprclean'),
            X('supported-model', filter='--subdirs=').children(
                'GNATtest execution mode'),
            X('extra-args', sections='-cargs').children(
                X('arg').children("%attr(ide_coverage'gnatcov_mode_switches)"),
                X('arg').children('--subdirs=%subdir'),
                X('arg', section='-cargs').children('-g'),
                X('arg', section='-cargs').children('-fdump-scos'),
                X('arg', section='-cargs').children('-fpreserve-control-flow'),
            )
        )
    ]

    BUILD_TARGETS = [
        X('target-model', name='gnatcov-build-main', category='').children(
            X('description').children('Build Main with the gnatcov switches'),
            X('command-line').children(
                X('arg').children('gprbuild')
            ),
            X('iconname').children('gps-build-all-symbolic'),
            X('switches', command='%(tool_name)s', columns='2', lines='2'),
        ),

        X('target', model='gnatcov-build-main', category='GNATcov',
          name='GNATcov Build Main', menu=PLUGIN_MENU).children(
            X('target-type').children('executable'),
            X('in-toolbar').children('FALSE'),
            X('in-menu').children('TRUE'),
            X('read-only').children('TRUE'),
            X('output-parsers').children(
                'output_chopper utf_converter console_writer end_of_build'),
            X('iconname').children('gps-build-all-symbolic'),
            X('launch-mode').children('MANUALLY'),
            X('command-line').children(
                X('arg').children('%builder'),
                X('arg').children('-P%PP'),
                X('arg').children('%subdirsarg'),
                X('arg').children('-s'),
                X('arg').children('-cargs'),
                X('arg').children('-g'),
                X('arg').children('-fdump-scos'),
                X('arg').children('-fpreserve-control-flow')
            )
        ),

        # Program execution under instrumented execution environment
        X('target-model', name='gnatcov-run', category='').children(
            X('description').children('Run under GNATcov for code coverage'),
            X('command-line').children(
                X('arg').children('gnatcov'),
                X('arg').children('run'),
            ),
            X('iconname').children('gps-build-all-symbolic'),
            X('switches', command='%(tool_name)s', columns='1', lines='1')
        ),

        X('target', model='gnatcov-run', category='GNATcov',
          name='Run under GNATcov', menu=PLUGIN_MENU).children(
            X('target-type').children('executable'),
            X('in-toolbar').children('FALSE'),
            X('in-menu').children('TRUE'),
            X('read-only').children('TRUE'),
            X('output-parsers').children(
                'output_chopper utf_converter console_writer end_of_build'),
            X('iconname').children('gps-build-all-symbolic'),
            X('launch-mode').children('MANUALLY'),
            X('command-line').children(
                X('arg').children('gnatcov'),
                X('arg').children('run'),
                X('arg').children('-P%PP'),
                X('arg').children('--recursive'),
                X('arg').children('%target'),
                X('arg').children('-c'),
                X('arg').children("%attr(ide_coverage'level_run,stmt)"),
                X('arg').children('-o'),
                X('arg').children('%TT.trace'),
                X('arg').children('%E'),
                X('arg').children("%attr(ide_coverage'switches_run)"),
            ),
        ),

        # Coverage report generation
        X('target-model', name='gnatcov-coverage', category='').children(
            X('description').children('Code coverage with GNATcov'),
            X('command-line').children(
                X('arg').children('gnatcov'),
                X('arg').children('coverage'),
                X('arg').children('-P%PP'),
                X('arg').children('--recursive'),
                X('arg').children('%target'),
                X('arg').children('--annotate=xcov'),
            ),
            X('iconname').children('gps-build-all-symbolic'),
            X('switches', command='%(tool_name)s', columns='1', lines='4'),
        ),

        X('target', model='gnatcov-coverage', category='GNATcov',
            name='Generate GNATcov Main Report',
            menu=PLUGIN_MENU).children(
            X('target-type').children('executable'),
            X('in-toolbar').children('FALSE'),
            X('in-menu').children('TRUE'),
            X('read-only').children('TRUE'),
            X('output-parsers').children(
                'output_chopper utf_converter console_writer end_of_build'),
            X('iconname').children('gps-build-all-symbolic'),
            X('launch-mode').children('MANUALLY'),
            X('command-line').children(
                X('arg').children('gnatcov'),
                X('arg').children('coverage'),
                X('arg').children('-P%PP'),
                X('arg').children('--recursive'),
                X('arg').children('%target'),
                X('arg').children('-c'),
                X('arg').children("%attr(ide_coverage'level_coverage,stmt)"),
                X('arg').children('--annotate=xcov+'),
                X('arg').children('--output-dir=%O'),
                X('arg').children('-T'),
                X('arg').children('%TT.trace'),
                X('arg').children("%attr(ide_coverage'switches_coverage)"),
            ),
        ),
    ]

    GNATCOV_DOCUMENTATION = [
        X('doc_path').children(
            os.path.join(gnatcov_install_dir, 'share',
                         'doc', 'gnatcoverage', 'html')
            if gnatcov_install_dir else
            None
        ),
        X('documentation_file').children(
            X('name').children('gnatcov.html'),
            X('descr').children("GNATcoverage User's Guide"),
            X('category').children('GNATcoverage'),
            X('menu', before='About').children(
                "/Help/GNATcoverage/GNATcoverage User's Guide"
            ),
        ),
    ]

    GNATEMU_DOCUMENTATION = [
        X('doc_path').children('share/doc/gnatemu/html'),
        X('documentation_file').children(
            X('name').children('gnatemulator.html'),
            X('descr').children('GNATemulator Documentation'),
            X('category').children('GNATcoverage'),
            X('menu', before='About').children(
                '/Help/GNATcoverage/GNATemulator Documentation'
            ),
        ),
    ]

    def __init__(self):
        # Create all custom things that do not require GPS' GUI to be ready
        # (i.e.: all but menus and hooks).
        for xml_nodes in (
            self.PROJECT_ATTRIBUTES, self.BUILD_MODES,
            self.GNATCOV_DOCUMENTATION, self.GNATEMU_DOCUMENTATION,
        ):
            GPS.parse_xml(list_to_xml(xml_nodes))

        # Create the GNATcoverage toolbar button
        self.create_toolbar_button()

        # Defer further initialization to when GPS is completely ready.
        GPS.Hook('gps_started').add(self.on_gps_started)

    def create_toolbar_button(self):
        workflows.create_target_from_workflow(
            target_name="Run GNATcoverage",
            workflow_name="run-gnatcov",
            workflow=self.run_gnatcov_wf,
            icon_name="gps-run-gnatcov-symbolic",
            parent_menu="/Build/Workflow/GNATcov/")

    def run_gnatcov_wf(self, main_name):
        # Build the project with GNATcov switches
        p = promises.TargetWrapper("GNATcov Build Main")
        r = yield p.wait_on_execute()
        if r is not 0:
            GPS.Console("Messages").write("Can't build the project with " +
                                          "the GNATcov switches", mode="error")
            return

        # Get the executable to analyze
        exe = str(GPS.File(main_name).executable_path)

        # Run GNATcov on it
        p = promises.TargetWrapper("Run under GNATcov")
        r = yield p.wait_on_execute(exe)
        if r is not 0:
            GPS.Console("Messages").write("GNATcov run failed ", mode="error")
            return

        # Generate and display the GNATcov Coverage Report
        p = promises.TargetWrapper("Generate GNATcov Main Report")
        r = yield p.wait_on_execute(exe)

    def on_gps_started(self, hook):
        # Now the parent menu is present, fill it with custom targets.
        GPS.parse_xml(list_to_xml(self.BUILD_TARGETS))

        GPS.Hook('compilation_finished').add(self.on_compilation_finished)

    def reload_gnatcov_data(self):
        """Clean the coverage report and reload it from the files."""

        # If needed, switch to GNATcov build mode.
        if GPS.Preference("Coverage-Toolchain").get() != 'Gnatcov':
            GPS.Preference("Coverage-Toolchain").set('Gnatcov')

        GPS.execute_action("coverage clear from memory")

        if GPS.Project.root().is_harness_project():
            a = GPS.CodeAnalysis.get("Coverage Report")
            original = GPS.Project.root().original_project().file()
            a.add_gcov_project_info(original)
        else:
            GPS.execute_action("coverage load data for all projects")

    def on_compilation_finished(self, hook, category,
                                target_name="", mode_name="", status=""):
        """Called whenever a compilation ends."""

        # If compilation failed, do nothing.
        if status:
            return

        if target_name in ["Generate GNATcov Main Report"]:
            self.reload_gnatcov_data()


# This plugin makes sense only if GNATcoverage is available.
if os_utils.locate_exec_on_path('gnatcov'):
    plugin = GNATcovPlugin()
