""" This plugin adds support for adareducer.
"""

import os
import shutil
import sys
import time

import GPS
from extensions.private.xml import X
from gs_utils import make_interactive
from modules import Module
from workflows import task_workflow
from workflows.promises import ProcessWrapper, TargetWrapper

PROJECT_TEMPLATE = """project Collected_Sandbox is

   for Source_Dirs use (".");
   for Object_Dir use ".";

   --  Complete this project file as needed for your reproducer

end Collected_Sandbox;
"""

ORACLE_TEMPLATE_DOC = """#
# Edit this oracle script as needed.
#
# The Ada reducer will reduce the Ada code as much as it can
# while this program returns 0.
#
# Some notes:
#   - make this script as fast as possible, making sure to exit
#     early if you can. For instance if you know that a certain
#     statement must be present in the code in order for the oracle
#     to pass, you might want to add a "grep" early in the script
#     to check for the presence oracle this statement.
#
#   - if you are using gprbuild, use -m2 to rely on source hashes
#     rather than timestamps when determining whether to rebuild,
#     since adareducer might modify files faster than the timestamp
#     granularity.
#

# Here's an example: reduce the code as much as you can while
# making sure it compiles.

"""

ORACLE_TEMPLATE_BASH = (
    ORACLE_TEMPLATE_DOC
    + """
gprbuild -m2 -q -P collected_sandbox.gpr || exit 1
"""
)

ORACLE_TEMPLATE_POWERSHELL = (
    ORACLE_TEMPLATE_DOC
    + """
gprbuild -m2 -q -P collected_sandbox.gpr
if (!$?) {exit 1}
"""
)

CONFIRMATION_MESSAGE = """The Ada reducer will start.

NOTE: this will modify source files in this project.

Ready?
"""


def list_to_xml(items):
    return "\n".join(str(i) for i in items)


class AdareducerPlugin(Module):

    # Define a build target
    BUILD_TARGETS = [
        X(
            "target",
            model="custom",
            category="_Adareducer_",
            name="adareducer",
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
                X("arg").children("gnatstudio_cli"),
                X("arg").children("adareducer"),
                X("arg").children("-P%PP"),
            ),
        ),
    ]

    def setup(self):
        # Create the build target
        GPS.parse_xml(list_to_xml(self.BUILD_TARGETS))

        # Create the actions
        make_interactive(
            self.collect_sandbox_project,
            category="Adareducer",
            name="collect sandbox project",
        )
        make_interactive(
            self.test_oracle_script,
            category="Adareducer",
            name="test oracle script",
        )
        make_interactive(
            self.launch_adareducer,
            category="Adareducer",
            name="launch adareducer",
        )

    def oracle_file(self):
        """Return the path to the expected oracle script."""
        return os.path.join(
            os.path.dirname(GPS.Project.root().file().name()),
            "oracle.sh" if sys.platform == "linux" else "oracle.ps1",
        )

    def launch_oracle_workflow(self, task):
        """Workflow to launch the oracle script and display the output."""
        oracle = self.oracle_file()

        # Craft the command
        if oracle.endswith(".sh"):
            cmd = ["bash", oracle]
        elif oracle.endswith(".ps1"):
            cmd = ["powershell", "-File", oracle]
        else:
            cmd = [oracle]

        # Clear the console and output the oracle command
        console = GPS.Console()
        console.clear()
        console.write(" ".join(cmd) + "\n")

        # Launch the process
        clock = time.time()
        p = ProcessWrapper(cmd)
        status, output = yield p.wait_until_terminate()

        # Write the output
        delta = time.time() - clock
        console.write(output + "\n")
        console.write(f"Time: {delta:.2f}s\n")
        console.write(f"Exit code: {status}\n", mode="text" if status == 0 else "error")

    def launch_adareducer_workflow(self, task):
        t = TargetWrapper("adareducer")
        yield t.wait_on_execute(extra_args=["--oracle_script", self.oracle_file()])

    def launch_adareducer(self):
        if GPS.MDI.yes_no_dialog(CONFIRMATION_MESSAGE):
            task_workflow("running adareducer", self.launch_adareducer_workflow)

    def test_oracle_script(self):
        """Create the oracle script if it doesn't exist, launch it if needed."""
        oracle = self.oracle_file()
        if os.path.exists(oracle):
            # The oracle file exists, run it
            task_workflow("running oracle", self.launch_oracle_workflow)
        else:
            self.create_oracle()
            GPS.EditorBuffer.get(GPS.File(self.oracle_file()))

    def create_oracle(self):
        oracle = self.oracle_file()
        with open(oracle, "w") as f:
            if oracle.endswith(".sh"):
                f.write(ORACLE_TEMPLATE_BASH)
            else:
                f.write(ORACLE_TEMPLATE_POWERSHELL)

    def collect_sandbox_project(self):
        """Collect the source files into a 'sandbox' project"""

        # By default, place the sandbox in obj/sandbox
        sandbox_dir = os.path.join(GPS.Project.root().object_dirs()[0], "sandbox")

        # Ask the user if they want another directory instead
        sandbox_dir = GPS.MDI.input_dialog(
            "Select the directory in which to collect the sources",
            f"Directory={sandbox_dir}",
        )[0]

        # Check the target location
        if os.path.exists(sandbox_dir):
            if len(os.listdir(sandbox_dir)) > 0:
                r = GPS.MDI.yes_no_dialog(
                    "This directory is not empty. Remove all contents now?"
                )
                if r:
                    shutil.rmtree(sandbox_dir)
                    os.mkdir(sandbox_dir)
                else:
                    return
        else:
            os.mkdir(sandbox_dir)

        # Copy over all the sources
        for x in GPS.Project.root().sources(recursive=True):
            shutil.copy(x.name(), sandbox_dir)

        # Create a project file
        prj = os.path.join(sandbox_dir, "collected_sandbox.gpr")
        with open(prj, "w") as f:
            f.write(PROJECT_TEMPLATE)

        # Offer to open the project file
        r = GPS.MDI.yes_no_dialog(
            "Source collection successful.\n\nSwitch to sandbox project?"
        )
        if r:
            GPS.Project.load(prj)

            # Create the initial oracle script
            self.create_oracle()

            # Open editors for the project file and the oracle file
            GPS.EditorBuffer.get(GPS.File(prj))
            GPS.EditorBuffer.get(GPS.File(self.oracle_file()))
