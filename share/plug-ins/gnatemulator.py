"""
This plug-in creates buttons on the toolbar to conveniently
debug, and run programs on GNATemulator.

The following is required:
 - the GNATemulator for your target should be present on the PATH, if not the
   buttons won't be displayed.
"""

import GPS
from modules import Module
import gps_utils.workflow as workflow
from gps_utils.workflow import WORKFLOW_PARAMETER
import gps_utils.promises as promise
from os_utils import locate_exec_on_path
from gps_utils.console_process import Console_Process


def log(msg):
    GPS.Logger("GNATemulator").log(msg)


class GNATemulator(Module):

    # a list of targets
    __buttons = []

    def get_gnatemu_name(self):
        target = GPS.get_target()
        if target:
            prefix = target + '-'
        else:
            prefix = ""

        return prefix + "gnatemu"

    def gnatemu_on_path(self):
        bin = self.get_gnatemu_name()

        gnatemu = locate_exec_on_path(bin)
        return gnatemu != ''

    def run_gnatemu(self, args):
        gnatemu = self.get_gnatemu_name()
        proj = GPS.Project.root()
        project_arg = "-P%s " % proj.file().name() if proj else ""
        var_args = GPS.Project.scenario_variables_cmd_line("-X")

        jargs = "%s %s %s" % (project_arg, var_args, " ".join(args))
        GPS.Console("Messages").write("Running in emulator: %s %s" %
                                      (gnatemu, jargs))

        #  - Open a console for each GNATemu run
        #  - Don't close the console when GNAtemu exits so we have time to see
        #    the results
        #  - GNATemu should be in the task manager
        Console_Process(gnatemu, args=jargs, force=True,
                        close_on_exit=False, task_manager=True)

    def __error_exit(self, msg=""):
        """ Emit an error and reset the workflows """
        GPS.Console("Messages").write(msg + " [workflow stopped]")

    def __show_button(self):
        """Initialize buttons and parameters.
        """

        # Show the buttons only if a gnatemu is available for the target
        if self.gnatemu_on_path():
            for b in self.__buttons:
                b.show()
        else:
            for b in self.__buttons:
                b.hide()

    ###############################
    # The following are workflows #
    ###############################

    def __emu_wf(self):
        """
        Workflow to build and run the program in the emulator.
        """

        # STEP 1.0 get main name
        f = yield WORKFLOW_PARAMETER
        if f is None:
            self.__error_exit(msg="Main not specified")
            return

        # STEP 1.5 Build it
        log("Building Main %s..." % f)
        builder = promise.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(f)
        if r0 is not 0:
            self.__error_exit(msg="Build error.")
            return

        log("... done.")

        # STEP 2 load with Emulator
        b = GPS.Project.root().get_executable_name(GPS.File(f))
        d = GPS.Project.root().object_dirs()[0]
        obj = d + b
        self.run_gnatemu([obj])

    def __emu_debug_wf(self):
        """
        Workflow to debug a program under the emulator.
        """

        # STEP 1.0 get main name
        f = yield WORKFLOW_PARAMETER
        if f is None:
            self.__error_exit(msg="Main not specified.")
            return

        # STEP 1.5 Build it
        log("Building Main %s..." % f)
        builder = promise.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(f)
        if r0 is not 0:
            self.__error_exit(msg="Build error.")
            return
        binary = GPS.Project.root().get_executable_name(GPS.File(f))

        log("... done.")

        # STEP 2 launch debugger

        debugger_promise = promise.DebuggerWrapper(GPS.File(binary))

        # block execution until debugger is free
        r3 = yield debugger_promise.wait_and_send(cmd="", block=False)
        if not r3:
            self.__error_exit("Could not initialize the debugger.")
            r3 = yield debugger_promise.wait_and_send(cmd="", block=False)
            return
        log("... done.")

        # STEP 3 load with Emulator
        # To have GNATemu console in the debugger perspective we have to start
        # GNATemu after gdb initialization.
        d = GPS.Project.root().object_dirs()[0]
        obj = d + binary
        self.run_gnatemu(["-g", obj])

        # STEP 4 target and run the program
        log("Sending debugger command to target the emulator...")
        r3 = yield debugger_promise.wait_and_send(
            cmd="target remote localhost:1234",
            timeout=4000)
        interest = "Remote debugging using localhost:1234"

        if interest not in r3:
            self.__error_exit("Could not connect to the target.")
            return

        log("... done.")

    # The followings are hooks:

    def gps_started(self):
        """
        When GPS start, add button (include cireteria there)
        """

        targets_def = [["Run with Emulator", "run-with-emulator",
                        self.__emu_wf, "gps-emulatorloading"],
                       ["Debug with Emulator", "debug-with-emulator",
                        self.__emu_debug_wf, "gps-emulatorloading-debug"]]

        for target in targets_def:

            # Create targets * 2:
            workflow.create_target_from_workflow(target[0], target[1],
                                                 target[2], target[3])

            b = GPS.BuildTarget(target[0])
            self.__buttons.append(b)

        self.__show_button()

    def project_view_changed(self):
        """
        When project view changes, add button (include cireteria there)
        """
        self.__show_button()
