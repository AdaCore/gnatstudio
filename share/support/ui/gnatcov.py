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
in all object and executable directories specified
the project hierarchy.

We expose a way to specify the coverage level of all commands
via the Project Properties, using GNATcoverage's Coverage project
package.

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

import json
import re
import shutil
import tempfile
import xml.sax.saxutils

import GPS
from extensions.private.xml import X
from gs_utils import interactive
from gs_utils.internal.dialogs import Project_Properties_Editor
from modules import Module
import os_utils
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
        path = os.path.join(gnatcov_install_dir, 'share', 'doc', name, 'html')
        if os.path.isdir(path):
            gnatcov_doc_path = path
            break

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

# The project attributes must be created when the plugin is loaded or they
# will not be found when opening the first project.
if gnatcov_path:
    GPS.parse_xml("""
        <tool name="GNATcoverage" package="Coverage" index="*" override="false"
        attribute="switches">
            <language>Ada</language>
            <switches>
                <title line="1" column="1">Coverage Level</title>
                <combo label="Level"
                    line="1"  column="1"
                    switch="--level"
                    separator="="
                    noswitch=""
                    tip="Used to specify the coverage level.">
                    <combo-entry label="stmt" value="stmt"/>
                    <combo-entry label="stmt+decision" value="stmt+decision"/>
                    <combo-entry label="stmt+mcdc" value="stmt+mcdc"/>
                    <combo-entry label="stmt+uc_mcdc" value="stmt+uc_mcdc"/>
                </combo>
            </switches>
        </tool>
""")


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
            X('command-help').children('{help}'),
            X('command-line').children(
                X('arg').children('gprbuild')
            ),
            X('iconname').children('gps-build-all-symbolic'),
            X('switches', command='%(tool_name)s', columns='2', lines='2'),
        ),

        # Program execution under instrumented execution environment
        X('target-model', name='gnatcov-run', category='').children(
            X('description').children('Run under GNATcov for code coverage'),
            X('command-help').children('{help}'),
            X('command-line').children(
                X('arg').children('gnatcov'),
                X('arg').children('run'),
            ),
            X('iconname').children('gps-build-all-symbolic'),
            X("switches", command="%(tool_name)s",
              columns="1", lines="1").children(
                X(
                    "combo",
                    line="1",
                    column="1",
                    noswitch="stmt",
                    separator="=",
                    switch="--level",
                    label="Coverage Level",
                    tip="""The coverage level to pass to gnatcov run."""
                ).children(
                    X('combo-entry', label='branch', value='branch'),
                    X('combo-entry', label='insn', value='insn'),
                    X('combo-entry', label='stmt', value='stmt'),
                    X('combo-entry', label='stmt+decision',
                      value='stmt+decision'),
                    X('combo-entry', label='stmt+mcdc', value='stmt+mcdc'),
                ),
            ),
        ),
        # Coverage report generation
        X('target-model', name='gnatcov-coverage', category='').children(
            X('description').children('Code coverage with GNATcov'),
            X('command-help').children('{help}'),
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
            X("switches",
              command="%(tool_name)s", columns="1", lines="1").children(
                X(
                    "combo",
                    line="1",
                    column="1",
                    noswitch="stmt",
                    separator="=",
                    switch="--level",
                    label="Coverage Level",
                    tip="""The coverage level to pass to gnatcov coverage."""
                ).children(
                    X('combo-entry', label='branch', value='branch'),
                    X('combo-entry', label='insn', value='insn'),
                    X('combo-entry', label='stmt', value='stmt'),
                    X('combo-entry', label='stmt+decision',
                      value='stmt+decision'),
                    X('combo-entry', label='stmt+mcdc', value='stmt+mcdc'),
                ),
            ),
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
                X('arg').children('-o'),
                X('arg').children('%O%T.trace'),
                X('arg').children('%E'),
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
                X('arg').children('--annotate=xcov+'),
                X('arg').children('--output-dir=%O'),
                X('arg').children('-T'),
                X('arg').children('%O%T.trace'),
                X('arg').children('%X'),
            ),
        ),
    ]

    SOURCE_TRACES_BUILD_TARGETS = [
        X('target', model='gnatcov-build-main', category='_GNATcov_',
          name='GNATcov Setup Instrumentation Runtime',
          menu=SOURCE_TRACES_MENU + '/Setup/').children(
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
                X('arg').children('setup'),
                X('arg').children('%target'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_rts_arg())'),
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.get_prefix_arg())')
            )
        ),

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
                X('arg').children(
                    '%python' +
                    '(gnatcov.GNATcovPlugin.' +
                    'get_runtime_project_arg())'),
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
                    'get_implicit_with_arg())')
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
                X('arg').children('--annotate=xcov+'),
                X('arg').children('--output-dir=%O'),
                X('arg').children('-T'),
                X('arg').children('%O%T.srctrace'),
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
        if not self.is_gnatcov_available:
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
        process = GPS.Process(["gnatcov", "--help"])
        help_msg = process.get_result()
        GPS.parse_xml(
            list_to_xml(self.BUILD_TARGET_MODELS).format(
                help=xml.sax.saxutils.escape(help_msg)
            )
        )
        self.update_worflow_build_targets()

        # Try to retrieve a prebuilt GNATcov runtime from the history
        global prebuilt_runtime_path
        prebuilt_runtime_path = GPS.History.get(
            RUNTIME_PATH_HIST_KEY, most_recent=True)

        GPS.Hook('compilation_finished').add(self.on_compilation_finished)

    def teardown(self):
        GNATcovPlugin.remove_secure_temp_dir()

    def project_view_changed(self):
        if self.is_gnatcov_available:
            self.update_worflow_build_targets()

    def update_worflow_build_targets(self):
        gnatcov_available = self.is_gnatcov_available
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

                GPS.parse_xml(list_to_xml(self.SOURCE_TRACES_BUILD_TARGETS[:5]))

                workflows.create_target_from_workflow(
                    target_name="Run instrumented main",
                    workflow_name="run-instrumented-main",
                    in_toolbar=False,
                    icon_name="gps-run-gnatcov-symbolic",
                    workflow=self.run_instrumented_main_wf,
                    parent_menu=SOURCE_TRACES_MENU + "/Run/")

                # Generate Report

                GPS.parse_xml(list_to_xml(self.SOURCE_TRACES_BUILD_TARGETS[5:]))

        if not gnatcov_available:
            if self.__run_gnatcov_wf_build_target:
                self.__run_gnatcov_wf_build_target.hide()

            if self.__run_gnatcov_instr_wf_build_target:
                self. __run_gnatcov_instr_wf_build_target.hide()

        elif not instrumentation_supported:
            if self.__run_gnatcov_instr_wf_build_target:
                self. __run_gnatcov_instr_wf_build_target.hide()

    @property
    def is_gnatcov_available(self):
        return os_utils.locate_exec_on_path('gnatcov') != ""

    @staticmethod
    def run_tool_version(exe):
        """
        Run the "`exe` --version" command and return its output.
        """
        return GPS.Process(exe + " --version").get_result()

    # Return the tool version as (major version, minor version)
    @classmethod
    def version(cls, exe, product_name):
        """Return the major version number of the given tool.

        Return None if we fail to detect the version.
        """
        out = cls.run_tool_version(exe)
        pattern = (
            re.escape(product_name)
            + r" ([0-9]+)\.[0-9]+w? \([0-9]{8}\)"
        )
        m = re.search(pattern, out)
        if m is None:
            return None

        return int(m.group(1))

    @classmethod
    def gnatcov_version(cls):
        return cls.version("gnatcov", "GNATcoverage")

    @classmethod
    def gprbuild_version(cls):
        return cls.version("gprbuild", "GPRBUILD Pro")

    @classmethod
    def is_binary_supported(cls):
        """Return whether binary traces are supported."""
        # Starting from GNATcov version 22.0, binary traces are no longer
        # supported in native setups. Assume they are not supported for native
        # setups if unable to detect the version (i.e. assume recent version).
        if GPS.get_target() != "":
            return True

        major = cls.gnatcov_version()
        return major is not None and major < 22

    @classmethod
    def is_instrumentation_supported(cls):
        """Return if gnatcov's instrumentation mode is supported."""
        # Check if GNATcov and GPRbuild are recent enough (21 releases and
        # after). Assume it is supported if unable to detect the version (i.e.
        # assume recent version).
        for exe, major in [
            ("gnatcov", cls.gnatcov_version()),
            ("gprbuild", cls.gprbuild_version()),
        ]:
            if major is None:
                continue

            if major < 21:
                GPS.Logger("GNATCOVERAGE").log(
                    f"instrumentation mode not supported due to an older {exe}"
                )
                return False

        return True

    # TODO: assume this predicate is true in GS 24.0.
    @classmethod
    def is_gnatcov_setup_supported(cls):
        """Return if the "gnatcov setup" command is supported."""
        # Check if GNATcov is recent enough (23 wavefronts and 24 releases or
        # later). Assume it is supported if unable to detect the version (i.e.
        # assume recent version).
        major = cls.gnatcov_version()
        return major is None or major >= 23

    @staticmethod
    def check_for_defined_level(cmds):
        """
        Check if the level is correcly specified in the root project for the given
        commands.
        This is done by checking for "--level" in the Coverage'Switches project
        attribute.
        """
        project = GPS.Project.root()

        @workflows.run_as_workflow
        def open_gnatcoverage_project_properties(text):
            """
            Open the Project Properties editor and go to the page
            defining the GNATcoverage settings.
            """

            editor = Project_Properties_Editor()
            yield editor.open_and_yield(wait_scan=False)
            yield editor.select("Build/Switches/GNATcoverage")

        # Check if the level is specified for all commands first (via the "*" index)
        if "--level" in project.get_attribute_as_string(
          "switches", package="coverage", index="*"):
            return

        # Check if we are missing the level for the given commands
        missing_cmds = []
        for cmd in cmds:
            if "--level" not in project.get_attribute_as_string(
              "switches", package="coverage", index=cmd):
                missing_cmds.append(cmd)

        # If we are missing the level, display a message in th Messsages view with
        # an hyperlink to open the GNATcoverage Project Properties page.
        if missing_cmds:
            console = GPS.Console()
            console.write(("warning: no level specified for the given commands: %s\n"
                           % (missing_cmds)))
            console.write("Please specify the coverage level via the ")
            console.insert_link("Coverage'Switches",
                                open_gnatcoverage_project_properties)
            console.write(" project attribute\n")

    def run_gnatcov_wf(self, main_name):
        # Check if the level is specified for the commands executed by the wf
        GNATcovPlugin.check_for_defined_level(cmds=["run", "coverage"])

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
    def run_instrumented_main_wf(self, main_name, generate=False):
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

            # TODO: suppress code below in GS 24.0.
            dump_channel = "bin-file"

        else:
            # Launch the instrumented executable through GNATemulator
            cmdargs = GPS.BuildTarget(
                "Run GNATemulator").get_expanded_command_line()
            cmdargs.append(exe)
            GPS.Console().write(' '.join(cmdargs) + "\n")
            gnatemu_promise = promises.ProcessWrapper(cmdargs=cmdargs)
            status, output = yield gnatemu_promise.wait_until_terminate(
                show_if_error=True)

            # TODO: suppress code below in GS 24.0.
            dump_channel = "base64-stdout"

        # TODO: suppress code below in GS 24.0.
        dump_trigger = None

        if self.is_gnatcov_setup_supported():
            # In that case, we use the gnatcov-instr.json file that gnatcov
            # instrument creates to know where traces were produced (dumped to a
            # binary file or dumped to the standard output).
            obj_dir = GPS.Project.root().object_dirs()[0]
            params_file = os.path.join(obj_dir, "gnatcov-instr.json")
            with open(params_file) as f:
                params = json.load(f)
                dump_channel = params["dump-channel"]
                dump_trigger = params["dump-trigger"]

        if dump_trigger == "manual":
            GPS.Console("Messages").write(
                "\nManual dump trigger is not supported in the GNAT Studio"
                " workflow. Please compute the coverage report manually and use"
                " the GNAT Studio action to load it.",
                mode="error"
            )
            return status

        if dump_channel == "base64-stdout":
            # Put the output in a file and use 'gnatcov extract-base64-trace'
            # to retrieve the traces information from it.
            out_filename = os.path.join(obj_dir, exe + ".out")

            with open(out_filename, "w") as f:
                f.write(output)
            extract_trace_cmd = ["gnatcov", "extract-base64-trace",
                                 out_filename, srctrace_filename]
            GPS.Console().write(' '.join(extract_trace_cmd) + "\n")
            status = GPS.Process(extract_trace_cmd).wait()

            if status != 0:
                GPS.Console("Messages").write(
                    "Could not extract traces info from executable's output",
                    mode="error")

        if status == 0 and generate:
            # Generate and display the GNATcov Coverage Report
            p = promises.TargetWrapper(
                    "Generate GNATcov Instrumented Main Report")
            yield p.wait_on_execute(exe, quiet=True)

        return status

    def run_gnatcov_with_instrumentation_wf(self, main_name):
        # Check if the level is specified for the commands executed by the wf
        GNATcovPlugin.check_for_defined_level(cmds=["instrument", "coverage"])

        # Get the executable to analyze
        exe = str(GPS.File(main_name).executable_path)

        # Don't build/install the GNATcov runtime if a prebuilt one has been
        # specified.
        if not prebuilt_runtime_path:
            # Use "gnatcov setup" if it is available; otherwise, fallback on
            # our old build + install process.
            if self.is_gnatcov_setup_supported():
                p = promises.TargetWrapper(
                    "GNATcov Setup Instrumentation Runtime"
                )
                r = yield p.wait_on_execute()
                if r != 0:
                    GPS.Console("Messages").write(
                        "GNATcov setup failed ", mode="error")
                    return

            else:
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
                        "GNATcov runtime installation failed ", mode="error")
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

        self.run_instrumented_main_wf(main_name, True)

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
            a = GPS.CodeAnalysis.get("Coverage")
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
        """
        Return the absolute path to the gnatcov instrumentation runtime project
        to use for the current target/runtime.
        """
        runtime_attr = GPS.get_runtime()

        # Locate the directory that contains the gnatcov instrumentation
        # runtime projects.
        gnatcov_path = os_utils.locate_exec_on_path("gnatcov")
        gnatcov_dir = os.path.dirname(gnatcov_path)
        rts_dir = os.path.join(
            gnatcov_dir, os.pardir, "share", "gnatcoverage", "gnatcov_rts"
        )

        default = os.path.join(rts_dir, "gnatcov_rts.gpr")
        full = os.path.join(rts_dir, "gnatcov_rts_full.gpr")

        # Pick the restricted profile for BB runtimes ("default"), the "full"
        # one otherwise (unless the full one does not exist, after the
        # gnatcov_rts merge):

        if ("ravenscar" in runtime_attr
                or "zfp" in runtime_attr
                or "light" in runtime_attr
                or "embedded" in runtime_attr
                or not os.path.exists(full)):
            return default
        else:
            return full

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
        Return the project option ("-P...") to use the gnatcov instrumentation
        runtime for the current target/runtime.
        """

        return "-P" + GNATcovPlugin.get_coverage_runtime_gpr_name()

    @staticmethod
    def get_dump_trigger_arg():
        if GNATcovPlugin.is_gnatcov_setup_supported():
            return ""

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

    # TODO: remove code below in GS 24.0.
    @staticmethod
    def get_dump_channel_arg():
        if GNATcovPlugin.is_gnatcov_setup_supported():
            return ""

        target_attr = GPS.get_target()

        if target_attr == "":
            return "--dump-channel=bin-file"
        else:
            return "--dump-channel=base64-stdout"

    @staticmethod
    def get_runtime_project_arg():
        if GNATcovPlugin.is_gnatcov_setup_supported():
            return (
                "--runtime-project=" +
                GNATcovPlugin.get_installed_coverage_runtime_project_path()
            )
        return ""

    @staticmethod
    def get_implicit_with_arg():
        return (
            "--implicit-with=" +
            GNATcovPlugin.get_installed_coverage_runtime_project_path()
        )

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
    def get_installed_coverage_runtime_project_path():
        if prebuilt_runtime_path:
            return prebuilt_runtime_path
        else:
            install_dir = os.path.join(
                GNATcovPlugin.get_secure_temp_dir(), "share", "gpr"
            )
            gpr_name = os.path.basename(
                GNATcovPlugin.get_coverage_runtime_gpr_name()
            )
            return os.path.join(install_dir, gpr_name)
