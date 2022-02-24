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

To use GNATcoverage with binary traces, click on the
Analyze/Coverage/GNATcoverage Binary Traces/Run all actions menu.
The following steps will be performed:
  1 - Build the executable using the standard mecanism
  2 - Run it to produce the trace file
  5 - Produce a coverage report

To use GNATcoverage with source traces (instrumentation), click on the
Analyze/Coverage/GNATcoverage Source Traces/Run all actions menu or the
'Run GNATcov' button, which is added to the main toolbar when the plugin is
enabled.
The following steps will be performed:
  1 - Build the GNATcoverage runtime, needed to instrument the executable
  2 - Call 'gnatcov instrument'
  3 - Build the instrumented main
  4 - Run it to produce the trace file
  5 - launch a first analysis using the menu

All these steps can be executed independently via the menu
Analyze/Coverage/GNATcoverage source traces.

You can also select a prebuilt runtime, that will be saved in GNATstudio
history, for later reuse.

Note: this plugin activates only when the command-line tool
"gnatcov" is found on the PATH.
"""

###########################################################################
# No user customization below this line
###########################################################################

import os.path

import GPS
from extensions.private.xml import X
from gs_utils import interactive
from modules import Module
import os_utils
import re
import shutil
import tempfile
import workflows.promises as promises
import workflows


PLUGIN_MENU = '/Analyze/Coverage/GNATcoverage'

BINARY_TRACES_MENU = PLUGIN_MENU + ' Binary Traces'
SOURCE_TRACES_MENU = PLUGIN_MENU + ' Source Traces'

TOOL_VERSION_REGEXP = re.compile(r"[a-zA-Z\s]+ ([0-9]*)\.?([0-9]*w?)")

RUNTIME_PATH_HIST_KEY = "gnatcov-prebuilt-runtime-path"


def list_to_xml(items):
    return '\n'.join(str(i) for i in items)


# Look for the installation prefix for the "gnatcov" binary
gnatcov_path = os_utils.locate_exec_on_path('gnatcov')
gnatcov_install_dir = (
    os.path.join(os.path.dirname(gnatcov_path), '..')
    if gnatcov_path else
    None
)

# From there, look for the directory that contain its documentation
gnatcov_doc_path = None
if gnatcov_install_dir:
    for name in ('gnatcoverage', 'gnatdas'):
        gnatcov_doc_path = os.path.join(
            gnatcov_install_dir, 'share', 'doc', name, 'html'
        )
        if not os.path.isdir(gnatcov_doc_path):
            gnatcov_doc_path = None

# Finally, also look for the index file. In legacy documentations, GNATcoverage
# was the only product documented, so we could just take the global doc index.
# Nowadays, the doc aggregates several tools, so take the gnatcov-specific
# index.
gnatcov_doc_index = None
if gnatcov_doc_path:
    for name in (
        os.path.join('gnatcov', 'gnatcov_part.html'),
        'gnatcov.html',
        'index.html',
    ):
        if os.path.exists(os.path.join(gnatcov_doc_path, name)):
            gnatcov_doc_index = name
            break


secure_temp_dir = None
# The secure temp directory used to build the GNATcov runtime in
# instrumentation mode.


prebuilt_runtime_path = None
# Used to store the prebuilt GNATcov runtime path, if the user specified
# one.

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

# The project attributes must be created when the plugin is loaded or they
# will not be found when opening the first project.
if gnatcov_path:
    GPS.parse_xml(list_to_xml(PROJECT_ATTRIBUTES))


class GNATcovPlugin(Module):

    # Keep this style name synchronized with Code_Coverage.GNATcov.

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

    BUILD_TARGET_MODELS = [
        X('target-model', name='gnatcov-build-main', category='').children(
            X('description').children('Build Main with the gnatcov switches'),
            X('command-line').children(
                X('arg').children('gprbuild')
            ),
            X('iconname').children('gps-build-all-symbolic'),
            X('switches', command='%(tool_name)s', columns='2', lines='2'),
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

        # Coverage report generation
        X('target-model', name='gnatcov-coverage', category='').children(
            X('description').children('Code coverage with GNATcov'),
            X('command-line').children(
                X('arg').children('gnatcov'),
                X('arg').children('coverage'),
                X('arg').children('%X'),
                X('arg').children('-P%PP'),
                X('arg').children('--recursive'),
                X('arg').children('%target'),
                X('arg').children('--annotate=xcov'),
            ),
            X('iconname').children('gps-build-all-symbolic'),
            X('switches', command='%(tool_name)s', columns='1', lines='4'),
        ),

    ]

    BINARY_TRACES_BUILD_TARGETS = [
        X('target', model='gnatcov-build-main', category='_GNATcov_',
          name='GNATcov Build Main',
          menu=BINARY_TRACES_MENU + '/Build/').children(
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

        X('target', model='gnatcov-run', category='_GNATcov_',
          name='Run under GNATcov',
          menu=BINARY_TRACES_MENU + '/Run/').children(
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
                X('arg').children('%O%T.trace'),
                X('arg').children('%E'),
                X('arg').children("%attr(ide_coverage'switches_run)"),
                X('arg').children('%X'),
            ),
        ),

        X('target', model='gnatcov-coverage', category='_GNATcov_',
            name='Generate GNATcov Main Report',
            menu=BINARY_TRACES_MENU + '/Generate Report/').children(
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
                X('arg').children('%O%T.trace'),
                X('arg').children("%attr(ide_coverage'switches_coverage)"),
                X('arg').children('%X'),
            ),
        ),
    ]

    SOURCE_TRACES_BUILD_TARGETS = [
        X('target', model='gnatcov-run', category='_GNATcov_',
          name='Run GNATcov with instrumentation',
          menu=SOURCE_TRACES_MENU + '/Instrumentation/').children(
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
                X('arg').children('instrument'),
                X('arg').children('-P%PP'),
                X('arg').children('%subdirsarg'),
                X('arg').children('--level'),
                X('arg').children("%attr(ide_coverage'level_run,stmt)"),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_dump_trigger_arg())'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_dump_channel_arg())'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_dump_filename_simple_arg())'),
                X('arg').children('%X'),
            ),
        ),

        X('target', model='gnatcov-build-main',
          category='_GNATcov_',
          name='GNATcov Build Coverage Runtime',
          menu=SOURCE_TRACES_MENU + '/Build/').children(
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
                X('arg').children('%X'),
                X('arg').children('-f'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_rts_arg())'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_coverage_runtime_project_arg())'),
                X('arg').children(
                    '%python' +
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
                X('arg').children('%X'),
                X('arg').children('%target'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_rts_arg())'),
                X('arg').children('-f'),
                X('arg').children('-p'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_coverage_runtime_project_arg())'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.get_relocate_build_tree_arg())'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.get_prefix_arg())')
            )
        ),

        X('target', model='gnatcov-build-main', category='_GNATcov_',
          name='GNATcov Build Instrumented Main',
          menu=SOURCE_TRACES_MENU + '/Build/').children(
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
                X('arg').children('%X'),
                X('arg').children('-p'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_rts_arg())'),
                X('arg').children('-P%PP'),
                X('arg').children('%subdirsarg'),
                X('arg').children('--src-subdirs=gnatcov-instr'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_installed_coverage_runtime_project_arg())')
            )
        ),

        X('target', model='gnatcov-coverage', category='_GNATcov_',
            name='Generate GNATcov Instrumented Main Report',
            menu=SOURCE_TRACES_MENU + '/Generate Report/').children(
            X('target-type').children('executable'),
            X('read-only').children('TRUE'),
            X('in-menu').children('TRUE'),
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
                X('arg').children('%O%T.srctrace'),
                X('arg').children("%attr(ide_coverage'switches_coverage)"),
                X('arg').children('%X'),
            ),

        ),
    ]
    GNATCOV_DOCUMENTATION = [
        X('doc_path').children(gnatcov_doc_path),
        X('documentation_file').children(
            X('name').children(gnatcov_doc_index),
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
            self.BUILD_MODES,
            self.GNATCOV_DOCUMENTATION, self.GNATEMU_DOCUMENTATION,
        ):
            GPS.parse_xml(list_to_xml(xml_nodes))

        # Update the GNATcoverage workflow Build Targets, creating them and
        # showing/hiding them appropriately. Also fill the custom targets.
        GPS.parse_xml(list_to_xml(self.BUILD_TARGET_MODELS))
        self.update_worflow_build_targets()

        # Try to retrieve a prebuilt GNATcov runtime from the history
        global prebuilt_runtime_path
        prebuilt_runtime_path = GPS.History.get(
            RUNTIME_PATH_HIST_KEY, most_recent=True)

        GPS.Hook('compilation_finished').add(self.on_compilation_finished)

    def teardown(self):
        GNATcovPlugin.remove_secure_temp_dir()

    def project_view_changed(self):
        if self.is_gnatcov_available():
            self.update_worflow_build_targets()

    def update_worflow_build_targets(self):
        gnatcov_available = self.is_gnatcov_available()
        instrumentation_supported = self.is_instrumentation_supported()

        if gnatcov_available and not self.__run_gnatcov_wf_build_target:
            if self.is_binary_supported():
                workflows.create_target_from_workflow(
                    target_name="Run GNATcoverage",
                    workflow_name="run-gnatcov",
                    workflow=self.run_gnatcov_wf,
                    in_toolbar=not instrumentation_supported,
                    icon_name="gps-run-gnatcov-symbolic",
                    parent_menu=BINARY_TRACES_MENU + "/Run All Actions/")

                self.__run_gnatcov_wf_build_target = \
                    GPS.BuildTarget("Run GNATcoverage")

                GPS.parse_xml(list_to_xml(self.BINARY_TRACES_BUILD_TARGETS))

            if instrumentation_supported:
                workflows.create_target_from_workflow(
                    target_name="Run GNATcoverage with instrumentation",
                    workflow_name="run-gnatcov-with-instrumentation",
                    in_toolbar=True,
                    icon_name="gps-run-gnatcov-symbolic",
                    workflow=self.run_gnatcov_with_instrumentation_wf,
                    parent_menu=SOURCE_TRACES_MENU + "/Run All Actions/")

                self.__run_gnatcov_instr_wf_build_target = \
                    GPS.BuildTarget("Run GNATcoverage with instrumentation")

                # We want the toolbar to be ordered according to the coverage
                # process:
                # GNATcoverage source traces
                #  -> Instrument
                #  -> Build
                #  -> Run
                #  -> Generate Report
                #
                # As the Run action is a a workflow, and is not included in
                # SOURCE_TRACES_BUILD_TARGETS, we have to incorporate it in the
                # middle. We thus first parse the XML corresponding to the
                # Instrument and Build steps, then parse the XML for the Run
                # workflow, and in the end, for the Generate Report build
                # target.

                # Instrument and Build

                GPS.parse_xml(list_to_xml(self.SOURCE_TRACES_BUILD_TARGETS[:4]))

                workflows.create_target_from_workflow(
                    target_name="Run instrumented main",
                    workflow_name="run-instrumented-main",
                    in_toolbar=False,
                    icon_name="gps-run-gnatcov-symbolic",
                    workflow=self.run_instrumented_main_wf,
                    parent_menu=SOURCE_TRACES_MENU + "/Run/")

                # Generate Report

                GPS.parse_xml(list_to_xml(self.SOURCE_TRACES_BUILD_TARGETS[4:]))

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

    # Return the tool version as (major version, minor version)
    def version(self, exe):
        latest_version = (23, 0)
        version_out = GPS.Process(exe + " --version").get_result()

        # Support a gnatcov built in dev mode
        if version_out == "GNATcoverage development-tree":
            return latest_version

        matches = TOOL_VERSION_REGEXP.findall(version_out.splitlines()[0])
        return matches[0]

    def is_binary_supported(self):
        # Starting from GNATcov version 22.0, binary traces are no longer
        # supported in native.

        try:
            version_major, _ = self.version("gnatcov")
        except Exception:
            # Can happen with the GS testuite if we use a fake gnatcov exe
            return True

        return GPS.get_target() != "" or int(version_major) < 22

    def is_instrumentation_supported(self):
        # Check if GNATcov and GPRbuild are recent enough (after the 21
        # release)
        for exe in 'gnatcov', 'gprbuild':
            try:
                version_major, version_minor = self.version(exe)
            except Exception as e:
                # Can happen with the GS testuite if we use a fake gnatcov exe
                GPS.Console().write(str(e))
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
        if r != 0:
            GPS.Console("Messages").write("Can't build the project with " +
                                          "the GNATcov switches", mode="error")
            return

        # Get the executable to analyze
        exe = str(GPS.File(main_name).executable_path)

        # Run GNATcov on it
        p = promises.TargetWrapper("Run under GNATcov")
        r = yield p.wait_on_execute(exe)
        if r != 0:
            GPS.Console("Messages").write("GNATcov run failed ", mode="error")
            return

        # Generate and display the GNATcov Coverage Report
        p = promises.TargetWrapper("Generate GNATcov Main Report")
        r = yield p.wait_on_execute(exe)

    @workflows.run_as_workflow
    def run_instrumented_main_wf(self, main_name):
        exe = str(GPS.File(main_name).executable_path)
        # Go to the object directory before executing the instrumented main: we
        # want to produce the trace file in the object dir and not in the
        # project's root directory
        obj_dir = GPS.Project.root().object_dirs()[0]
        GPS.cd(obj_dir)

        # Clean the previous trace file if it exists (the run can fails and
        # then the trace file will not be overwritten: it will show outdated
        # data)
        srctrace_filename = os.path.join(obj_dir, exe + ".srctrace")
        try:
            os.remove(srctrace_filename)
        except FileNotFoundError:
            pass

        # Run the instrumented main (through GNATemulator for cross targets)
        # it will generate the new trace file.
        target = GPS.get_target()
        if target == "":
            cmdargs = [exe]
            p = promises.ProcessWrapper(cmdargs)

            GPS.Console().write(' '.join(cmdargs))
            status, output = yield p.wait_until_terminate(show_if_error=True)
            if status != 0:
                GPS.Console("Messages").write(
                    "Failed to execute main with status " + str(status))
        else:
            # Launch the instrumented executable through GNATemulator
            cmdargs = GPS.BuildTarget(
                "Run GNATemulator").get_expanded_command_line()
            cmdargs.append(exe)
            GPS.Console().write(' '.join(cmdargs) + "\n")
            gnatemu_promise = promises.ProcessWrapper(cmdargs=cmdargs)
            status, output = yield gnatemu_promise.wait_until_terminate(
                show_if_error=True)

            # Put the output in a file and use 'gnatcov extract-base64-trace'
            # to retrieve the traces information from it
            out_filename = os.path.join(obj_dir, exe + ".out")

            with open(out_filename, "w") as f:
                f.write(output)
            extract_trace_cmd = ["gnatcov", "extract-base64-trace",
                                 out_filename, srctrace_filename]
            GPS.Console().write(' '.join(extract_trace_cmd) + "\n")
            status = GPS.Process(extract_trace_cmd).wait()

            if status != 0:
                GPS.Console().write(
                    "Could not extract traces info from executable's output",
                    mode="error")
        return status

    def run_gnatcov_with_instrumentation_wf(self, main_name):
        # Get the executable to analyze
        exe = str(GPS.File(main_name).executable_path)

        # Don't build/install the GNATcov runtime if a prebuilt one has been
        # specified.
        if not prebuilt_runtime_path:
            # Build the coverage runtime
            p = promises.TargetWrapper("GNATcov Build Coverage Runtime")
            r = yield p.wait_on_execute(quiet=True)
            if r != 0:
                GPS.Console("Messages").write(
                    "GNATcov runtime build failed ", mode="error")
                return

            # Install the coverage runtime
            p = promises.TargetWrapper("GNATcov Install Coverage Runtime")
            r = yield p.wait_on_execute(quiet=True)
            if r != 0:
                GPS.Console("Messages").write(
                    "GNATcov runtime build failed ", mode="error")
                return
        else:
            GPS.Console().write(
                "\nPrebuilt runtime is used: %s\n" % prebuilt_runtime_path)

        # Run GNATcov with instrumentation on it
        p = promises.TargetWrapper("Run GNATcov with instrumentation")
        r = yield p.wait_on_execute(exe, quiet=True)
        if r != 0:
            GPS.Console("Messages").write("GNATcov instrumentation failed ",
                                          mode="error")
            return

        # Build the instrumented main
        p = promises.TargetWrapper("GNATcov Build Instrumented Main")
        r = yield p.wait_on_execute(quiet=True)
        if r != 0:
            GPS.Console("Messages").write("Can't build the project with " +
                                          "the GNATcov switches", mode="error")
            return

        self.run_instrumented_main_wf(main_name)

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
    @interactive(category="GNATcov",
                 menu=(SOURCE_TRACES_MENU +
                       "/Select prebuilt runtime"),
                 description=("Select the .gpr project of an "
                              + "installed  GNATcoverage prebuilt runtime."),
                 name="select gnatcov runtime")
    def set_prebuilt_runtime_action():
        """
        Create an action and a menu to be able to select an installed prebuilt
        GNATcoverage runtime.
        The path to this runtime is stored in the history, making it persistant
        accross sessions.
        """
        global prebuilt_runtime_path
        hist_path = GPS.History.get(RUNTIME_PATH_HIST_KEY,
                                    most_recent=True)

        base_dir = ""
        if hist_path:
            base_dir = os.path.dirname(hist_path)

        file = GPS.MDI.file_selector(base_dir=base_dir)
        prebuilt_runtime_path = file.path
        GPS.History.add(RUNTIME_PATH_HIST_KEY, file.path)

    @staticmethod
    def get_secure_temp_dir():
        """
        Create a secure temp directory using tempfile.mkdtemp.
        This directory should be deleted manually after usage (see the
        GNATcovPlugin.remove_secure_temp_dir function for that).
        """
        global secure_temp_dir

        if not secure_temp_dir:
            secure_temp_dir = tempfile.mkdtemp()

        return secure_temp_dir

    @staticmethod
    def remove_secure_temp_dir():
        """
        Remove the secure temp dir created via
        GNATcovPlugin.get_secure_temp_dir, if it exists.
        """
        global secure_temp_dir

        if secure_temp_dir:
            try:
                shutil.rmtree(secure_temp_dir)
            except Exception as e:
                GPS.Logger("GNATCOVERAGE").log(
                    "exception when removing temp dir: %s" % e)
            secure_temp_dir = None

    @staticmethod
    def get_coverage_runtime_gpr_name():
        runtime_attr = GPS.get_runtime()

        # Pick the restricted profile for BB runtimes, the "full" one
        # otherwise:

        if ("ravenscar" in runtime_attr
                or "zfp" in runtime_attr
                or "light" in runtime_attr
                or "embedded" in runtime_attr):
            return "gnatcov_rts.gpr"
        else:
            return "gnatcov_rts_full.gpr"

    @staticmethod
    def get_rts_arg():
        """
        Return the needed --RTS option for gnatcov command lines.
        """
        runtime_attr = GPS.get_runtime()

        if runtime_attr == "":
            return ""
        else:
            return "--RTS=%s" % runtime_attr

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

        runtime_gpr = GNATcovPlugin.get_coverage_runtime_gpr_name()

        return "-P" + os.path.join(runtime_dir, runtime_gpr)

    @staticmethod
    def get_dump_trigger_arg():
        runtime_attr = GPS.get_runtime()

        # If we have a BB runtime profile around, pick the closest
        # plausible match. Assume atexit is usable otherwise:

        if ("ravenscar" in runtime_attr
                or "light-tasking" in runtime_attr
                or "embedded" in runtime_attr):
            return "--dump-trigger=ravenscar-task-termination"
        elif ("zfp" in runtime_attr
              or "light" in runtime_attr):
            return "--dump-trigger=main-end"
        else:
            return "--dump-trigger=atexit"

    @staticmethod
    def get_dump_channel_arg():
        target_attr = GPS.get_target()

        if target_attr == "":
            return "--dump-channel=bin-file"
        else:
            return "--dump-channel=base64-stdout"

    @staticmethod
    def get_dump_filename_simple_arg():
        target_attr = GPS.get_target()

        if target_attr == "":
            return "--dump-filename-simple"
        else:
            return ""

    @staticmethod
    def get_relocate_build_tree_arg():
        return "--relocate-build-tree=" + GNATcovPlugin.get_secure_temp_dir()

    @staticmethod
    def get_prefix_arg():
        return "--prefix=" + GNATcovPlugin.get_secure_temp_dir()

    @staticmethod
    def get_installed_coverage_runtime_project_arg():
        if prebuilt_runtime_path:
            return "--implicit-with=%s" % prebuilt_runtime_path
        else:
            return "--implicit-with=" + \
                os.path.join(GNATcovPlugin.get_secure_temp_dir(), "share",
                             "gpr",
                             GNATcovPlugin.get_coverage_runtime_gpr_name())
