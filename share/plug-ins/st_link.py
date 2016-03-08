"""
This plug-in creates buttons on the toolbar to conveniently
flash and debug programs for the STM32F4* boards.

The utility program st-util must be present on the PATH for
the buttons to be made visible. This utility is included in
recent Windows-based versions of GNAT for the arm-eabi targets,
or can be downloaded from https://github.com/texane/stlink and
built. In addition, the utility st-flash is required to be
on the path in order to flash memory as a separate operation.
Note that the USB driver for these utility programs must be
installed in order for them to operate correctly, but this
plug-in is not concerned with that aspect.

"""

import GPS
from modules import Module
from target_connector import TargetConnector
import workflows
import workflows.promises as promises
from gps_utils.console_process import Console_Process


def msg_is(msg):
    GPS.Console("Messages").write(msg + "\n")


def uses_stm32(prj):
    """ Search the project to see if it uses the STM32 boards
    """
    s = prj.get_attribute_as_string(package="Builder",
                                    attribute="Default_Switches",
                                    index="Ada")
    if "stm32" in s:
        return True

    s = prj.get_attribute_as_string(package="Builder",
                                    attribute="Switches",
                                    index="Ada")
    if "stm32" in s:
        return True

    s = prj.get_attribute_as_string("runtime", index="Ada")
    if "stm32" in s:
        return True

    return False


class BoardLoader(Module):

    # The build targets that have been created lazily. See comments in
    # gnatemulator.py
    __buildTargets = []

    # The build target associated with the 'st-util' command line
    __connector = None

    # The ProcessWrapper instance used to spawn the 'st-util' tool
    # and the associated promises
    __connection = None

    def __error_exit(self, msg=""):
        """ Emit an error and reset the workflows """
        GPS.Console("Messages").write(
            msg + " [workflow stopped]",
            mode="error")

    def __reset_all(self, manager_delete=True, connection_delete=True):
        """ Reset the workflows """
        msg_is("Resetting the connection")
        if self.__connection is not None and connection_delete:
            self.__connection.terminate()
            self.__connection = None

            interest = "st-util"
            for i in GPS.Task.list():
                if interest in i.name():
                    i.interrupt()

    def __create_targets_lazily(self):
        active = uses_stm32(GPS.Project.root())

        if not self.__connector and active:
            self.__connector = TargetConnector("st-util", [])

        if not self.__buildTargets and active:
            workflows.create_target_from_workflow(
                "Flash to Board", "flash-to-board", self.__flash_wf,
                "gps-boardloading-flash-symbolic")
            self.__buildTargets.append(GPS.BuildTarget("Flash to Board"))

            workflows.create_target_from_workflow(
                "Debug on Board", "debug-on-board", self.__debug_wf,
                "gps-boardloading-debug-symbolic")
            self.__buildTargets.append(GPS.BuildTarget("Debug on Board"))

        if active:
            for b in self.__buildTargets:
                b.show()
        else:
            for b in self.__buildTargets:
                b.hide()

    ###############################
    # The following are workflows #
    ###############################

    def __flash_wf(self, main_name):
        """Workflow to build and flash the program on the board.
        """

        if main_name is None:
            self.__error_exit(msg="Could not find the name of the main.")
            return

        builder = promises.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(main_name)
        if r0 is not 0:
            self.__error_exit(msg="Build error.")
            return

        msg_is("Creating the binary (flashable) image.")
        b = GPS.Project.root().get_executable_name(GPS.File(main_name))
        d = GPS.Project.root().object_dirs()[0]
        obj = d + b
        binary = obj + ".bin"
        cmd = ["arm-eabi-objcopy", "-O", "binary", obj, binary]
        try:
            con = promises.ProcessWrapper(cmd)
        except:
            self.__error_exit("Could not launch executable arm-eabi-objcopy.")
            return

        r1 = yield con.wait_until_terminate()
        if r1 is not 0:
            self.__error_exit("arm-eabi-objcopy returned an error.")
            return

        msg_is("Flashing image to board.")
        cmd = ["st-flash", "write", binary, "0x8000000"]
        try:
            con = promises.ProcessWrapper(cmd, spawn_console=True)
        except:
            self.__error_exit("Could not connect to the board.")
            return

        r2 = yield con.wait_until_match(
            "Starting verification of write complete",
            120000)
        r3 = yield con.wait_until_match(
            "Flash written and verified! jolly good!",
            500)

        if not (r2 and r3):
            self.__error_exit(msg="Could not flash the executable.")
            con.terminate()
            return

        msg_is("Flashing complete. You may need to reset (or cycle power).")

    def __debug_wf(self, main_name):
        """
        Workflow to build, flash and debug the program on the real board.
        """

        # Reset the connection if still alive
        if self.__connection is not None:
            self.__reset_all()

        if main_name is None:
            self.__error_exit(msg="Main not specified")
            return

        # Build the executable
        builder = promises.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(main_name)
        if r0 is not 0:
            self.__error_exit("Build error.")
            return

        # Switch directly to the "Debug" perspective so that the
        # 'st-util' console is still visible when spawning the debugger.
        GPS.MDI.load_perspective("Debug")

        # Launch st-util with its associated console
        cmd = self.__connector.get_command_line()
        msg_is("Launching st-util.")
        self.__connection = promises.ProcessWrapper(cmd, spawn_console=True)
        r1 = yield self.__connection.wait_until_match(
            "Listening at",
            1000)

        if not r1:
            self.__error_exit(msg="Could not connect to the device.")
            return

        # Spawn the debugger on the executable and load it
        msg_is("Launching debugger.")
        b = GPS.Project.root().get_executable_name(GPS.File(main_name))
        debugger_promise = promises.DebuggerWrapper(GPS.File(b))
        r2 = yield debugger_promise.wait_and_send(
            cmd="load",
            block=True)

        if not r3:
            self.__error_exit("Connection Lost. "
                              + "Please check the USB connection and restart.")
            self.__reset_all()
            return

    def setup(self):
        """
        When setting up the module, create target and buildTargets.
        """
        GPS.Hook("debugger_terminated").add(self.debugger_terminated)
        self.__create_targets_lazily()

    def project_view_changed(self):
        self.__create_targets_lazily()

    def debugger_terminated(self, hookname, debugger):
        """
        When debugger terminates, terminate the connection.
        """
        self.__reset_all()
