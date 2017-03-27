"""
This plugin creates buttons on the toolbar to conveniently
debug, and run programs on GNATemulator.

The following is required:
 - the GNATemulator for your target should be present on the PATH, if not the
   buttons won't be displayed.
"""

import GPS
from modules import Module
import workflows.promises as promises
import workflows
from os_utils import locate_exec_on_path
from gps_utils.console_process import Console_Process
import os


project_attributes = """
  <project_attribute
   package="emulator"
   name="Debug_Port"
   label="Debug port"
   editor_page="GNATemulator"
   hide_in="wizard library_wizard"
   description="Port used by GNATemulator to debug."
  >
    <string/>
  </project_attribute>
"""

# This has to be done at GPS start, before the project is actually loaded.
GPS.parse_xml(project_attributes)


def log(msg):
    GPS.Logger("GNATemulator").log(msg)


class GNATemulator(Module):

    # List of targets
    # These are created lazily the first time we find the necessary tools on
    # the command line. This is done so that we do not have to toggle the
    # visibility of these build targets too often, since that also trigger
    # the reparsing of Makefiles, for instance, and a refresh of all GUI
    # elements related to any build target.
    __buildTargets = []

    def __create_targets_lazily(self):
        active = self.gnatemu_on_path()

        if not self.__buildTargets and active:
            targets_def = [
                ["Run with Emulator", "run-with-emulator",
                    self.__emu_wf, "gps-emulatorloading-run-symbolic"],
                ["Debug with Emulator", "debug-with-emulator",
                    self.__emu_debug_wf, "gps-emulatorloading-debug-symbolic"]]

            for target in targets_def:
                workflows.create_target_from_workflow(
                    target[0], target[1], target[2], target[3],
                    parent_menu='/Build/Emulator/%s/' % target[0])
                self.__buildTargets.append(GPS.BuildTarget(target[0]))

        if active:
            for b in self.__buildTargets:
                b.show()
        else:
            for b in self.__buildTargets:
                b.hide()

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
        project_arg = "-P%s" % proj.file().path if proj else ""
        var_args = GPS.Project.scenario_variables_cmd_line("-X")

        if var_args:
            command = [gnatemu, project_arg, var_args] + args
        else:
            command = [gnatemu, project_arg] + args

        GPS.Console("Messages").write("Running in emulator: %s" %
                                      (' '.join(command)))
        #  - Open a console for each GNATemu run
        #  - Don't close the console when GNAtemu exits so we have time to see
        #    the results
        #  - GNATemu should be in the task manager
        Console_Process(command=command, force=True,
                        close_on_exit=False, task_manager=True)

    def __error_exit(self, msg=""):
        """ Emit an error and reset the workflows """
        GPS.Console("Messages").write(
            msg + " [workflow stopped]",
            mode="error")

    ###############################
    # The following are workflows #
    ###############################

    def __emu_wf(self, main_name):
        """
        Workflow to build and run the program in the emulator.
        """

        if main_name is None:
            self.__error_exit(msg="Main not specified")
            return

        # STEP 1.5 Build it
        log("Building Main %s..." % main_name)
        builder = promises.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(main_name)
        if r0 is not 0:
            self.__error_exit(msg="Build error.")
            return

        log("... done.")

        # STEP 2 load with Emulator
        self.run_gnatemu([GPS.File(main_name).executable_path.path])

    def __emu_debug_wf(self, main_name):
        """
        Workflow to debug a program under the emulator.
        """

        # STEP 1.0 get main name
        if main_name is None:
            self.__error_exit(msg="Main not specified.")
            return

        # STEP 1.5 Build it
        log("Building Main %s..." % main_name)
        builder = promises.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(main_name)
        if r0 is not 0:
            self.__error_exit(msg="Build error.")
            return
        binary = GPS.File(main_name).executable_path.path

        log("... done.")

        # STEP 2 Switch to the "Debug" perspective To have GNATemu console in
        # the debugger perspective.

        GPS.MDI.load_perspective("Debug")

        # STEP 2 load with Emulator
        debug_port = GPS.Project.root().get_attribute_as_string(
            package="Emulator", attribute="Debug_Port")

        # TODO: remove this fall-back once GNATemulator supports the
        # new 'Debug_Port' attribute (Fabien's task)
        if debug_port == "":
            debug_port = "1234"

        self.run_gnatemu(["--freeze-on-startup",
                          "--gdb=%s" % debug_port,
                          binary])

        log("... done.")

        # STEP 3 launch the debugger
        debugger_promise = promises.DebuggerWrapper(
            GPS.File(binary),
            remote_target="localhost:" + debug_port,
            remote_protocol="remote")

        # block execution until debugger is free
        r3 = yield debugger_promise.wait_and_send(block=True)
        if not r3:
            self.__error_exit("Could not initialize the debugger.")
            return

        log("... done.")

    def setup(self):
        self.__create_targets_lazily()

    def project_view_changed(self):
        self.__create_targets_lazily()
