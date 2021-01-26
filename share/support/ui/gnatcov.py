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
GNATcoverage session would be to click on the 'Run GNATcoverage'
toolbar button or do the following steps manually:
  1 - switch to the "gnatcov" Build Mode in the toolbar
  2 - build the executable using the standard mechanism
  3 - launch a first run using the menu
      Analyze->Coverage->GNATcoverage->Run
  4 - launch a first analysis using the menu
      Analyze->Coverage->GNATcoverage->Generate Report with gnatcov
  5 - edit the code or the test driver, then rerun
      steps 2, 3, 4

In addition, to use GNATcoverage with instrumentation, click on the
Analyze/Coverage/GNATcoverage/Instrumentation menu.
The following steps will be performed:
  1 - Build the GNATcoverage runtime, needed to instrument the executable
  2 - Call 'gnatcov instrument'
  3 - Build the instrumented main
  4 - Run it to produce the trace file
  5 - launch a first analysis using the menu
      Analyze->Coverage->GNATcoverage->Generate Report with gnatcov

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
from modules import Module
import os_utils
import re
import tempfile
import workflows.promises as promises
import workflows


PLUGIN_MENU = '/Analyze/Coverage/GNATcoverage'

TOOL_VERSION_REGEXP = re.compile("[a-zA-Z\s]+ ([0-9]*)\.?([0-9]*w?)")


def list_to_xml(items):
    return '\n'.join(str(i) for i in items)


gnatcov_path = os_utils.locate_exec_on_path('gnatcov')
gnatcov_install_dir = (
    os.path.join(os.path.dirname(gnatcov_path), '..')
    if gnatcov_path else
    None
)


class GNATcovPlugin(Module):

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
            X('extra-args', sections='-cargs:Ada -cargs:C').children(
                X('arg').children("%attr(ide_coverage'gnatcov_mode_switches)"),
                X('arg').children('--subdirs=%subdir'),
                X('arg', section='-cargs:Ada').children('-g'),
                X('arg', section='-cargs:Ada').children('-fdump-scos'),
                X('arg', section='-cargs:Ada').children(
                    '-fpreserve-control-flow'),
                X('arg', section='-cargs:C').children('-g'),
                X('arg', section='-cargs:C').children('-fdump-scos'),
                X('arg', section='-cargs:C').children(
                    '-fpreserve-control-flow'),
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

        X('target', model='gnatcov-build-main', category='_GNATcov_',
          name='GNATcov Build Main',
          menu=PLUGIN_MENU + '/Build/').children(
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
                X('arg').children('%X'),
                X('arg').children('-cargs:Ada'),
                X('arg').children('-g'),
                X('arg').children('-fdump-scos'),
                X('arg').children('-fpreserve-control-flow'),
                X('arg').children('-cargs:C'),
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

        X('target', model='gnatcov-run', category='_GNATcov_',
          name='Run under GNATcov',
          menu=PLUGIN_MENU + '/Run/').children(
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
                X('arg').children('%X'),
            ),
        ),

        X('target', model='gnatcov-run', category='_GNATcov_',
          name='Run GNATcov with instrumentation').children(
            X('target-type').children('executable'),
            X('in-toolbar').children('FALSE'),
            X('in-menu').children('FALSE'),
            X('read-only').children('TRUE'),
            X('output-parsers').children(
                'output_chopper utf_converter console_writer end_of_build'),
            X('iconname').children('gps-build-all-symbolic'),
            X('launch-mode').children('MANUALLY'),
            X('command-line').children(
                X('arg').children('gnatcov'),
                X('arg').children('instrument'),
                X('arg').children('-P%PP'),
                X('arg').children('%subdirsarg'),
                X('arg').children('--level'),
                X('arg').children("%attr(ide_coverage'level_run,stmt)"),
                X('arg').children('--dump-trigger=atexit'),
                X('arg').children('%X'),
            ),
        ),

        X('target', model='gnatcov-build-main',
          category='_GNATcov_',
          name='GNATcov Build Coverage Runtime').children(
            X('target-type').children('executable'),
            X('in-toolbar').children('FALSE'),
            X('in-menu').children('FALSE'),
            X('read-only').children('TRUE'),
            X('output-parsers').children(
                'output_chopper utf_converter console_writer end_of_build'),
            X('iconname').children('gps-build-all-symbolic'),
            X('launch-mode').children('MANUALLY'),
            X('command-line').children(
                X('arg').children('%builder'),
                X('arg').children('-f'),
                X('arg').children(
                    '%python' + \
                    '(gnatcov.GNATcovPlugin.' + \
                    'get_coverage_runtime_project_arg())'),
                X('arg').children(
                    '%python' + \
                    '(gnatcov.GNATcovPlugin.get_relocate_build_tree_arg())'),
            )
        ),

        X('target', model='gnatcov-build-main',
          category='_GNATcov_',
          name='GNATcov Install Coverage Runtime').children(
            X('target-type').children('executable'),
            X('in-toolbar').children('FALSE'),
            X('in-menu').children('FALSE'),
            X('read-only').children('TRUE'),
            X('output-parsers').children(
                'output_chopper utf_converter console_writer end_of_build'),
            X('iconname').children('gps-build-all-symbolic'),
            X('launch-mode').children('MANUALLY'),
            X('command-line').children(
                X('arg').children('gprinstall'),
                X('arg').children('-f'),
                X('arg').children('-p'),
                X('arg').children(
                    '%python' + \
                    '(gnatcov.GNATcovPlugin.' + \
                    'get_coverage_runtime_project_arg())'),
                X('arg').children(
                    '%python' + \
                    '(gnatcov.GNATcovPlugin.get_relocate_build_tree_arg())'),
                X('arg').children(
                    '%python' + \
                    '(gnatcov.GNATcovPlugin.get_prefix_arg())')
            )
        ),

        X('target', model='gnatcov-build-main', category='_GNATcov_',
          name='GNATcov Build Instrumented Main').children(
            X('target-type').children('executable'),
            X('in-toolbar').children('FALSE'),
            X('in-menu').children('FALSE'),
            X('read-only').children('TRUE'),
            X('output-parsers').children(
                'output_chopper utf_converter console_writer end_of_build'),
            X('iconname').children('gps-build-all-symbolic'),
            X('launch-mode').children('MANUALLY'),
            X('command-line').children(
                X('arg').children('%builder'),
                X('arg').children('-f'),
                X('arg').children('-p'),
                X('arg').children('-P%PP'),
                X('arg').children('%subdirsarg'),
                X('arg').children('--src-subdirs=gnatcov-instr'),
                X('arg').children(
                    '%python' + \
                    '(gnatcov.GNATcovPlugin.' + \
                    'get_installed_coverage_runtime_project_arg())')
            )
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

        X('target', model='gnatcov-coverage', category='_GNATcov_',
            name='Generate GNATcov Main Report',
            menu=PLUGIN_MENU + '/Generate Report/').children(
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
                X('arg').children('%X'),
            ),
        ),

        X('target', model='gnatcov-coverage', category='_GNATcov_',
            name='Generate GNATcov Instrumented Main Report').children(
            X('target-type').children('executable'),
            X('read-only').children('TRUE'),
            X('in-menu').children('FALSE'),
            X('output-parsers').children(
                'output_chopper utf_converter console_writer end_of_build'),
            X('iconname').children('gps-build-all-symbolic'),
            X('launch-mode').children('MANUALLY'),
            X('command-line').children(
                X('arg').children('gnatcov'),
                X('arg').children('coverage'),
                X('arg').children('-P%PP'),
                X('arg').children('%subdirsarg'),
                X('arg').children('%target'),
                X('arg').children('-c'),
                X('arg').children("%attr(ide_coverage'level_coverage,stmt)"),
                X('arg').children('--annotate=xcov+'),
                X('arg').children('--output-dir=%O'),
                X('arg').children('-T'),
                X('arg').children('%TT.srctrace'),
                X('arg').children("%attr(ide_coverage'switches_coverage)"),
                X('arg').children('%X'),
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

    __run_gnatcov_wf_build_target = None
    # The 'Run GNATcoverage' workflow Build Target

    __run_gnatcov_instr_wf_build_target = None
    # The 'Run GNATcoverage with instrumentation' workflow Build Target

    def setup(self):
        # This plugin makes sense only if GNATcoverage is available.
        if not self.is_gnatcov_available():
            return

        # Create all custom things that do not require GPS' GUI to be ready
        # (i.e.: all but menus and hooks).
        for xml_nodes in (
            self.PROJECT_ATTRIBUTES, self.BUILD_MODES,
            self.GNATCOV_DOCUMENTATION, self.GNATEMU_DOCUMENTATION,
        ):
            GPS.parse_xml(list_to_xml(xml_nodes))

        # Update the GNATcoverage workflow Build Targets, creating them and
        # showing/hiding them appropriately
        self.update_worflow_build_targets()

        # Now the parent menu is present, fill it with custom targets.
        GPS.parse_xml(list_to_xml(self.BUILD_TARGETS))

        GPS.Hook('compilation_finished').add(self.on_compilation_finished)

    def project_view_changed(self):
        if self.is_gnatcov_available():
            self.update_worflow_build_targets()

    def update_worflow_build_targets(self):
        gnatcov_available = self.is_gnatcov_available()
        instrumentation_supported = self.is_instrumentation_supported()

        if gnatcov_available and not self.__run_gnatcov_wf_build_target:
            workflows.create_target_from_workflow(
                target_name="Run GNATcoverage",
                workflow_name="run-gnatcov",
                workflow=self.run_gnatcov_wf,
                icon_name="gps-run-gnatcov-symbolic",
                parent_menu="/Build/Workflow/GNATcov/")

            self.__run_gnatcov_wf_build_target = \
                GPS.BuildTarget("Run GNATcoverage")

            if instrumentation_supported:
                workflows.create_target_from_workflow(
                    target_name="Run GNATcoverage with instrumentation",
                    workflow_name="run-gnatcov-with-instrumentation",
                    in_toolbar=False,
                    workflow=self.run_gnatcov_with_instrumentation_wf,
                    parent_menu=PLUGIN_MENU + "/Intrumentation/")

                self.__run_gnatcov_instr_wf_build_target = \
                    GPS.BuildTarget("Run GNATcoverage with instrumentation")

        if not gnatcov_available:
            if self.__run_gnatcov_wf_build_target:
                self.__run_gnatcov_wf_build_target.hide()

            if self.__run_gnatcov_instr_wf_build_target:
                self. __run_gnatcov_instr_wf_build_target.hide()

        elif not instrumentation_supported:
            if self.__run_gnatcov_instr_wf_build_target:
                self. __run_gnatcov_instr_wf_build_target.hide()

    def is_gnatcov_available(self):
        return os_utils.locate_exec_on_path('gnatcov') != ""

    def is_instrumentation_supported(self):
        # Check if GNATcov and GPRbuild are recent enough (after the 21
        # release)
        for exe in 'gnatcov', 'gprbuild':

            try:
                version_out = GPS.Process(
                    exe + " --version").get_result().splitlines()[0]

                matches = TOOL_VERSION_REGEXP.findall(version_out)
                version_major, version_minor = matches[0]

            except Exception:
                # Can happen with the GS testuite if we use a fake gnatcov exe
                return False

            if not (int(version_major) >= 22 or
                    (int(version_major) == 21 and version_minor[-1] != "w")):
                GPS.Logger("GNATCOVERAGE").log(
                    "instrumentation mode not " +
                    "supported due to an older %s" % exe)
                return False

        return True

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

    def run_gnatcov_with_instrumentation_wf(self, main_name):

        # Get the executable to analyze
        exe = str(GPS.File(main_name).executable_path)

        # Build the coverage runtime
        p = promises.TargetWrapper("GNATcov Build Coverage Runtime")
        r = yield p.wait_on_execute(quiet=True)
        if r is not 0:
            GPS.Console("Messages").write(
                "GNATcov runtime build failed ", mode="error")
            return

        # Install the coverage runtime
        p = promises.TargetWrapper("GNATcov Install Coverage Runtime")
        r = yield p.wait_on_execute(quiet=True)
        if r is not 0:
            GPS.Console("Messages").write(
                "GNATcov runtime build failed ", mode="error")
            return
        # Run GNATcov with instrumentation on it
        p = promises.TargetWrapper("Run GNATcov with instrumentation")
        r = yield p.wait_on_execute(exe, quiet=True)
        if r is not 0:
            GPS.Console("Messages").write("GNATcov run failed ", mode="error")
            return

        # Build the instrumented main
        p = promises.TargetWrapper("GNATcov Build Instrumented Main")
        r = yield p.wait_on_execute(quiet=True)
        if r is not 0:
            GPS.Console("Messages").write("Can't build the project with " +
                                          "the GNATcov switches", mode="error")
            return

        # Go to the object directory before executing the instrumented main: we
        # want to produce the trace file in the object dir and not in the
        # project's root directory
        obj_dir = GPS.Project.root().object_dirs()[0]
        GPS.cd(obj_dir)

        # Build the instrumented main
        p = promises.ProcessWrapper(cmdargs=[exe])
        r = yield p.wait_until_terminate()

        # Generate and display the GNATcov Coverage Report
        p = promises.TargetWrapper("Generate GNATcov Instrumented Main Report")
        r = yield p.wait_on_execute(exe, quiet=True)

    def reload_gnatcov_data(self):
        """Clean the coverage report and reload it from the files."""

        # If needed, switch to GNATcov build mode.
        pref_name = ("Coverage-Toolchain"
                     if "ENABLE_GCOV" in os.environ
                     else "Coverage-Toolchain-Internal")

        if GPS.Preference(pref_name).get() != 'Gnatcov':
            GPS.Preference(pref_name).set('Gnatcov')

        GPS.execute_action("coverage clear from memory")

        if GPS.Project.root().is_harness_project():
            a = GPS.CodeAnalysis.get("Coverage Report")
            original = GPS.Project.root().original_project().file()
            a.add_gcov_project_info(original)
        else:
            GPS.execute_action("coverage load data for all projects")

    def on_compilation_finished(self, hook, category,
                                target_name="", mode_name="", status="",
                                *args):
        """Called whenever a compilation ends."""

        # If compilation failed, do nothing.
        if status:
            return

        if target_name in ["Generate GNATcov Main Report",
                           "Generate GNATcov Instrumented Main Report"]:
            self.reload_gnatcov_data()

    @staticmethod
    def get_coverage_runtime_project_arg():
        """
        Return the path of the coverage runtime bundled with the gnatcov
        installation.
        This runtime is needed to use gnatcov with instrumentation.
        """
        gnatcov_path = os_utils.locate_exec_on_path('gnatcov')
        gnatcov_dir = os.path.dirname(gnatcov_path)
        runtime_dir = os.path.join(gnatcov_dir, os.pardir,
                                   "share", "gnatcoverage", "gnatcov_rts")

        return "-P" + os.path.join(runtime_dir, "gnatcov_rts_full.gpr")

    @staticmethod
    def get_relocate_build_tree_arg():
        return "--relocate-build-tree=" + tempfile.gettempdir()

    @staticmethod
    def get_prefix_arg():
        return "--prefix=" + tempfile.gettempdir()

    @staticmethod
    def get_installed_coverage_runtime_project_arg():
        return "--implicit-with=" + \
            os.path.join(tempfile.gettempdir(), "share",
                         "gpr", "gnatcov_rts_full.gpr")
