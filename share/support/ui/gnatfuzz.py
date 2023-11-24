""" This plugin adds support for GNATfuzz.

This is activated when "gnatfuzz" is found on the PATH.

This adds
  - a toplevel menu for launching GNATfuzz operations
  - corresponding actions
  - workflows to
       - analyze a project to identify fuzzing closures
       - generate a fuzzing harness from a subprogram
         and switch to the fuzzing harness project
       - launch a fuzzing session on
"""

import os.path

import os
import glob
import json
import shutil
import time

import GPS
from extensions.private.xml import X
from gs_utils import make_interactive
from modules import Module
import os_utils
import workflows.promises as promises
import workflows

from gnatfuzz_view import get_gnatfuzz_view

from gnatfuzz_test_cases_view import get_gnatfuzz_test_case_view

FUZZ_MONITOR_TIMEOUT = 1000
# milliseconds between filesystem polling while fuzzing

FUZZ_TASK_NAME = "Fuzzing"


def list_to_xml(items):
    return "\n".join(str(i) for i in items)


gnatfuzz_path = os_utils.locate_exec_on_path("gnatfuzz")
gnatfuzz_install_dir = (
    os.path.join(os.path.dirname(gnatfuzz_path), "..") if gnatfuzz_path else None
)


class GNATfuzzPlugin(Module):
    """The main support plugin for GNATfuzz"""

    # Define some build targets
    BUILD_TARGETS = [
        X("target-model", name="gnatfuzz-generate-model", category="").children(
            X("description").children("Launch gnatfuzz generate"),
            X("command-line").children(),
            X("iconname").children("gps-build-all-symbolic"),
            X("switches", command="gnatfuzz", columns="2", lines="2").children(
                X(
                    "field",
                    line="1",
                    column="1",
                    label="Output directory",
                    switch="-o",
                    tip="the directory in which to generate the harness",
                )
            ),
        ),
        X("target-model", name="gnatfuzz-fuzz-model", category="").children(
            X("description").children("Launch gnatfuzz fuzz"),
            X("command-line").children(),
            X("iconname").children("gps-build-all-symbolic"),
            X("switches", command="gnatfuzz", columns="2", lines="1").children(
                X(
                    "spin",
                    line="1",
                    column="2",
                    label="Fuzzing cores",
                    separator="=",
                    switch="--cores",
                    default="0",
                    min="0",
                    max="32",
                    tip="The number of cores to use. Use 0 for automatic.",
                ),
                X(
                    "combo",
                    line="1",
                    column="1",
                    label="AFL mode",
                    separator="=",
                    switch="--afl-mode",
                    default="PERSIST",
                    tip="The AFL run mode. See the AFL++ documentation.",
                ).children(
                    X("combo-entry", label="PLAIN", value="afl_plain"),
                    X("combo-entry", label="PERSIST", value="afl_persist"),
                    # "DEFER" and "DEFER_AND_PERSIST" are not available yet;
                    # uncomment these lines when they are available.
                    #    X("combo-entry", label="DEFER", value="afl_defer"),
                    #    X(
                    #        "combo-entry",
                    #        label="DEFER_AND_PERSIST",
                    #        value="afl_defer_and_persist",
                    #    ),
                ),
                X(
                    "check",
                    line="1",
                    column="1",
                    label="Disable CMPLOG (aka RedQueen)",
                    switch="--no-cmplog",
                    default="off",
                    tip="Use this to deactivate the CMPLOG feature",
                ),
            ),
        ),
        X(
            "target",
            model="custom",
            category="_GNATfuzz_",
            name="gnatfuzz analyze project",
        ).children(
            X("target-type").children(""),
            X("in-toolbar").children("FALSE"),
            X("in-menu").children("FALSE"),
            X("read-only").children("TRUE"),
            X("output-parsers").children(
                "output_chopper utf_converter console_writer end_of_build"
            ),
            X("iconname").children("gps-build-all-symbolic"),
            X("launch-mode").children("MANUALLY"),
            X("command-line").children(
                X("arg").children("gnatfuzz"),
                X("arg").children("analyze"),
                X("arg").children("-P%PP"),
                X("arg").children("%subdirsarg"),
                X("arg").children("%X"),
            ),
        ),
        X(
            "target",
            model="custom",
            category="_GNATfuzz_",
            name="gnatfuzz analyze file",
        ).children(
            X("target-type").children(""),
            X("in-toolbar").children("FALSE"),
            X("in-menu").children("FALSE"),
            X("read-only").children("TRUE"),
            X("output-parsers").children(
                "output_chopper utf_converter console_writer end_of_build"
            ),
            X("iconname").children("gps-build-all-symbolic"),
            X("launch-mode").children("MANUALLY"),
            X("command-line").children(
                X("arg").children("gnatfuzz"),
                X("arg").children("analyze"),
                X("arg").children("-P%PP"),
                X("arg").children("%subdirsarg"),
                X("arg").children("%X"),
                X("arg").children("-S"),
                X("arg").children("%F"),
            ),
        ),
        X(
            "target",
            model="gnatfuzz-fuzz-model",
            category="_GNATfuzz_",
            name="gnatfuzz fuzz",
            menu="",
        ).children(
            X("target-type").children(""),
            X("in-toolbar").children("FALSE"),
            X("in-menu").children("FALSE"),
            X("read-only").children("TRUE"),
            X("output-parsers").children(
                "output_chopper utf_converter console_writer end_of_build"
            ),
            X("iconname").children("gps-build-all-symbolic"),
            X("launch-mode").children("MANUALLY_WITH_DIALOG"),
            X("command-line").children(
                X("arg").children("gnatfuzz"),
                X("arg").children("fuzz"),
                X("arg").children("-P%PP"),
                X("arg").children("%subdirsarg"),
            ),
        ),
        X(
            "target",
            model="gnatfuzz-fuzz-model",
            category="_GNATfuzz_",
            name="gnattest fuzz",
            menu="",
        ).children(
            X("target-type").children(""),
            X("in-toolbar").children("FALSE"),
            X("in-menu").children("FALSE"),
            X("read-only").children("TRUE"),
            X("iconname").children("gps-build-all-symbolic"),
            X("launch-mode").children("MANUALLY_WITH_DIALOG"),
            X("command-line").children(
                X("arg").children("gnatfuzz"),
                X("arg").children("fuzz"),
                X("arg").children("%subdirsarg"),
            ),
        ),
        X(
            "target",
            model="gnatfuzz-generate-model",
            category="_GNATfuzz_",
            name="gnatfuzz generate",
            menu="",
        ).children(
            X("target-type").children(""),
            X("in-toolbar").children("FALSE"),
            X("in-menu").children("FALSE"),
            X("read-only").children("TRUE"),
            X("output-parsers").children(
                "output_chopper utf_converter console_writer end_of_build"
            ),
            X("iconname").children("gps-build-all-symbolic"),
            X("launch-mode").children("MANUALLY_WITH_DIALOG"),
            X("command-line").children(
                X("arg").children("gnatfuzz"),
                X("arg").children("generate"),
                X("arg").children("-P%PP"),
                X("arg").children("%subdirsarg"),
                X("arg").children("%X"),
            ),
        ),
        X(
            "target",
            model="gnatfuzz-generate-model",
            category="_GNATfuzz_",
            name="gnattest gnatfuzz generate",
            menu="",
        ).children(
            X("target-type").children(""),
            X("in-toolbar").children("FALSE"),
            X("in-menu").children("FALSE"),
            X("read-only").children("TRUE"),
            X("output-parsers").children(
                "output_chopper utf_converter console_writer end_of_build"
            ),
            X("iconname").children("gps-build-all-symbolic"),
            X("launch-mode").children("MANUALLY_WITH_DIALOG"),
            X("command-line").children(
                X("arg").children("gnatfuzz"),
                X("arg").children("generate"),
                X("arg").children("%subdirsarg"),
                X("arg").children("%X"),
            ),
        ),
    ]

    def setup(self):
        # This plugin makes sense only if GNATfuzz is available:
        # return immediately if not.
        if not os_utils.locate_exec_on_path("gnatfuzz"):
            return

        # These fields are set when the project is a harness project
        self.user_project = None  # The original user project
        self.output_dir = None  # The gnatfuzz output dir

        # Create the build targets
        GPS.parse_xml(list_to_xml(self.BUILD_TARGETS))

        ref_menu = "Analyze"

        # Create the actions
        make_interactive(
            self.gnatfuzz_analyze_project,
            filter=self.not_harness_project,
            category="GNATfuzz",
            name="gnatfuzz analyze project workflow",
            menu="/GNATfuzz/Analyze Project",
            before=ref_menu,
        )
        make_interactive(
            self.gnatfuzz_analyze_file,
            filter=self.not_harness_project,
            category="GNATfuzz",
            name="gnatfuzz analyze file workflow",
            menu="/GNATfuzz/Analyze File",
            before=ref_menu,
        )
        make_interactive(
            self.gnatfuzz_generate,
            category="GNATfuzz",
            name="gnatfuzz generate workflow",
        )
        make_interactive(
            self.gnatfuzz_fuzz_start_stop,
            filter=self.is_harness_project,
            category="GNATfuzz",
            name="gnatfuzz fuzz workflow",
            menu="/GNATfuzz/Start\\/Stop Fuzzing Session",
            before=ref_menu,
        )
        make_interactive(
            self.switch_to_user_project,
            filter=self.is_harness_project,
            category="GNATfuzz",
            name="gnatfuzz switch to user project",
            menu="/GNATfuzz/Switch to User Project",
            before=ref_menu,
        )

        # Call the project changed hook to refresh the harness flags
        self.project_view_changed()

    def switch_to_user_project(self):
        """Switch back from the harness project to the user project"""
        if self.user_project is not None:
            GPS.Project.load(self.user_project)

    def teardown(self):
        """Inherited"""
        # Nothing to do
        pass

    def is_harness_project(self, context):
        return self.user_project is not None

    def not_harness_project(self, context):
        return not self.is_harness_project(context)

    def project_view_changed(self):
        """React to a project view change"""
        project_dir = os.path.dirname(GPS.Project.root().file().name())
        config_file = os.path.join(project_dir, "fuzz_config.json")

        if os.path.exists(config_file):
            # A config file has been found: this is a harness project

            # Read the contents of the config field
            with open(config_file, "r") as f:
                decoded = json.load(f)
            self.user_project = decoded["user_project"]
            self.output_dir = decoded["output_directory"]
        else:
            # This is not a harness project
            self.user_project = None
            self.output_dir = None

    def error(self, msg):
        """Convenience function to log an error in the Messages"""
        GPS.Console("Messages").write(msg + "\n", mode="error")

    ###########
    # Analyze #
    ###########

    def on_fuzzable_subprogram_click(self, message):
        """React to a click on a "fuzzable subprogram" message box"""
        # This is akin to a compilation: save everything that needs saving
        GPS.MDI.save_all()

        output_dir = os.path.join(GPS.Project.root().object_dirs()[0], "fuzz_harness")

        if os.path.exists(output_dir):
            shutil.rmtree(output_dir)

        analyze_report_file = self.path_to_analyze_json()
        if not os.path.exists(analyze_report_file):
            self.error(f"Analyze file not found: {analyze_report_file}")
            return

        # Launch "gnatfuzz generate"
        GPS.BuildTarget("gnatfuzz generate").execute(
            extra_args=[
                "-o",
                output_dir,
                "--analysis",
                analyze_report_file,
                "--subprogram-id",
                str(message.analyze_id),
            ],
            synchronous=True,
        )
        # TODO: make this a workflow, in case this takes a long time.

        harness_project = os.path.join(output_dir, "fuzz_testing", "fuzz_test.gpr")
        if os.path.exists(harness_project):
            r = GPS.MDI.yes_no_dialog(
                "Harness generation successful.\n\nSwitch to harness project?"
            )

            if r:
                GPS.Project.load(harness_project)

    def create_messages_from_analyze_json_entry(self, entry):
        """Create a GS Messages from one toplevel entry in "fuzzable_subprograms"
        in analyze.json
        """

        file = entry["source_filename"]
        line = entry["start_line"]
        if "id" in entry:
            # If there is an "id", this is a non-generic case

            m = GPS.Message(
                category="Fuzzable Subprograms",
                file=GPS.File(file),
                line=line,
                column=1,
                text="Fuzzable subprogram",
                show_on_editor_side=True,
                show_in_locations=True,
                auto_jump_to_first=False,
            )
            m.set_subprogram(
                self.on_fuzzable_subprogram_click,
                "gps-compile-symbolic",
                "Generate fuzz harness",
            )
            m.__setattr__("analyze_id", entry["id"])

        elif "instantiations" in entry:
            # This is a generic case: create a message with the chain
            if len(entry["instantiations"]) == 0:
                self.error("analyze.json: 'instantiations' empty")

            for sub in entry["instantiations"]:
                id = sub["id"]
                # First go through the chain to craft a label for the
                # toplevel message
                labels = []
                chain = sub["instantiation_chain"]
                if len(chain) == 0:
                    self.error("analyze.json: 'instantiation_chain' empty")

                chain.reverse()  #
                for chain_entry in chain:
                    labels.append(chain_entry["label"])
                text = "Fuzzable subprogram: " + ":".join(labels)
                m = GPS.Message(
                    category="Fuzzable Subprograms",
                    file=GPS.File(file),
                    line=line,
                    column=1,
                    text=text,
                    show_on_editor_side=True,
                    show_in_locations=True,
                    auto_jump_to_first=False,
                )
                m.set_subprogram(
                    self.on_fuzzable_subprogram_click,
                    "gps-compile-symbolic",
                    "Generate fuzz harness: " + ":".join(labels),
                )
                m.__setattr__("analyze_id", id)
                for chain_entry in chain:
                    label = chain_entry["label"]
                    chain_line = chain_entry["start_line"]
                    chain_filename = chain_entry["source_filename"]
                    base = os.path.basename(chain_filename)
                    sub_msg = f"... instantiated in {label} at {base}:{chain_line}"
                    # Create a nested message for each entry in the chain
                    m.create_nested_message(
                        GPS.File(chain_filename), chain_line, 1, sub_msg
                    )
        else:
            self.error("analyze.json entry without 'id' nor 'instantiations'")

    def path_to_analyze_json(self):
        """Return the path to analyze.json"""
        return os.path.join(
            GPS.Project.root().object_dirs()[0], "gnatfuzz", "analyze.json"
        )

    def process_analyze_messages(self):
        """Process the analyze.json file"""
        # Find the analyze report
        analyze_report_file = self.path_to_analyze_json()
        if not os.path.exists(analyze_report_file):
            self.error(f"Analyze file not found: {analyze_report_file}")
            return

        # Open the analyze report
        with open(analyze_report_file, "r") as f:
            try:
                decoded = json.load(f)
            except Exception:
                self.error(f"{analyze_report_file} corrupted")

        if "fuzzable_subprograms" not in decoded:
            self.error(f"{analyze_report_file} missing 'fuzzable_subprograms'")
            return

        # Create messages
        for entry in decoded["fuzzable_subprograms"]:
            self.create_messages_from_analyze_json_entry(entry)

    def gnatfuzz_analyze_project_workflow(self, task):
        """Workflow for 'gnatfuzz analyze project'."""

        # Launch the analyze target in the background
        p = promises.TargetWrapper("gnatfuzz analyze project")
        r = yield p.wait_on_execute()
        if r != 0:
            self.error("gnatfuzz analyze returned nonzero")
            return
        self.process_analyze_messages()

    def gnatfuzz_analyze_file_workflow(self, task):
        """Workflow for 'gnatfuzz analyze'."""

        # Launch the analyze target in the background
        p = promises.TargetWrapper("gnatfuzz analyze file")
        r = yield p.wait_on_execute()
        if r != 0:
            self.error("gnatfuzz analyze returned nonzero")
            return
        self.process_analyze_messages()

    def clear_analyze_messages(self):
        """Clear messages generated by 'gnatfuzz analyze'"""
        for m in GPS.Message.list(category="Fuzzable Subprograms"):
            m.remove()

    def gnatfuzz_analyze_project(self):
        """Action to launch the 'gnatfuzz analyze' workflow"""
        self.clear_analyze_messages()
        workflows.task_workflow(
            "gnatfuzz analyze", self.gnatfuzz_analyze_project_workflow
        )

    def gnatfuzz_analyze_file(self):
        self.clear_analyze_messages()
        """Action to launch the 'gnatfuzz analyze' workflow on a file"""
        workflows.task_workflow("gnatfuzz analyze", self.gnatfuzz_analyze_file_workflow)

    ############
    # Generate #
    ############

    def gnatfuzz_generate_workflow(self, task):
        """The 'gnatfuzz generate' workflow"""
        p = promises.TargetWrapper("gnatfuzz generate")
        r = yield p.wait_on_execute()
        if r != 0:
            GPS.Console("Messages").write(
                "gnatfuzz generate returned nonzero", mode="error"
            )
            return

    def gnatfuzz_generate(self):
        """Action to launch the 'gnatfuzz generate' workflow"""
        workflows.task_workflow("gnatfuzz generate", self.gnatfuzz_generate_workflow)

    ########
    # Fuzz #
    ########

    def gnatfuzz_fuzz_workflow(self, task):
        """The 'gnatfuzz fuzz' workflow"""

        # *** IMPORTANT ***
        # Ensure that we clear the GNATfuzz views before we launch them
        # If the views are closed, clearing is still safe and has no
        # effect. However, if we open the views before clearing them we
        # invoke a race condition where new test cases are loaded into
        # the view before the clear command is sent. This results in
        # the initial tests incorrectly getting removed from the
        # view.

        # Clear the GNATfuzz views
        GPS.execute_action("clear GNATfuzz fuzz crashes view")
        GPS.execute_action("clear GNATfuzz test cases view")

        # Move away the previous fuzzing session dir

        fuzz_session_dir = os.path.join(self.output_dir, "fuzz_testing", "session")
        if os.path.exists(fuzz_session_dir):
            shutil.rmtree(fuzz_session_dir)

        # Generate the -X switches
        args = []
        for variable, value in GPS.Project.scenario_variables().items():
            # We pass all -X switches except the ones that are internal
            # to gnatfuzz.
            if not (variable.startswith("GNATFUZZ") or variable == "AFL_MODE"):
                args.append(f"-X{variable}={value}")

        args.extend(
            [
                f"--corpus-path={self.output_dir}/fuzz_testing/starting_corpus",
                f"--stop-criteria={self.output_dir}"
                "/fuzz_testing/user_configuration/stop_criteria.xml",
            ]
        )

        GPS.BuildTarget("gnatfuzz fuzz").execute(
            extra_args=args,
            synchronous=False,
        )

        # Create a CodeAnalysis object to store the coverage data
        a = GPS.CodeAnalysis.get("gnatfuzz")

        xcov_files = {}  # Keys: full path, values: timestamp

        # Launch the GNATfuzz views
        GPS.execute_action("open GNATfuzz fuzz crashes view")
        GPS.execute_action("open GNATfuzz test cases view")

        # Monitor the disk for the presence of xcov files

        while True:
            # This is interrupted by the user calling the menu again,
            # in which case the Task will be removed: see at the bottom
            # of the loop.
            yield promises.timeout(FUZZ_MONITOR_TIMEOUT)

            if not os.path.exists(fuzz_session_dir):
                self.error(f"fuzz session directory {fuzz_session_dir} not found")
                self.stop_fuzz()
                break

            # Monitor for coverage files
            found_xcov_files = glob.glob(
                os.path.join(fuzz_session_dir, "coverage_output", "*.xcov")
            )
            for xcov in found_xcov_files:
                (mode, ino, dev, nlink, uid, gid, size, atime, mtime, ctime) = os.stat(
                    xcov
                )
                timestamp = time.ctime(mtime)
                if xcov not in xcov_files or xcov_files[xcov] != timestamp:
                    xcov_files[xcov] = timestamp
                    base = os.path.basename(xcov)[:-5]
                    a.add_gcov_file_info(
                        GPS.File(base), GPS.File(xcov), raise_window=False
                    )
                    a.show_file_coverage_info(GPS.File(base))

            # Monitor for crashes
            view = get_gnatfuzz_view()
            if view is not None:
                view.refresh()

            # Monitor for new testcases
            test_case_view = get_gnatfuzz_test_case_view()
            if test_case_view is not None:
                test_case_view.refresh()

            # The end condition
            tasks = [t for t in GPS.Task.list() if t.name() == "gnatfuzz fuzz"]
            if len(tasks) == 0:
                break

        return

    def is_fuzz_running(self):
        """Return True if "gnatfuzz fuzz" is running"""
        tasks = [t for t in GPS.Task.list() if t.name() == "gnatfuzz fuzz"]
        return len(tasks) > 0

    def stop_fuzz(self):
        tasks = [t for t in GPS.Task.list() if t.name() == "gnatfuzz fuzz"]
        if len(tasks) > 0:
            tasks[0].interrupt()

    def gnatfuzz_fuzz_start_stop(self):
        """Action to start/stop the 'gnatfuzz fuzz' workflow"""

        # Check whether we have a "gnatfuzz fuzz" process running: if so,
        # interrupt it, and the workflow should terminate.
        if self.is_fuzz_running():
            self.stop_fuzz()
        else:
            workflows.task_workflow(FUZZ_TASK_NAME, self.gnatfuzz_fuzz_workflow)
