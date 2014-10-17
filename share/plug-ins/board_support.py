"""
This plug-in creates buttons on the toolbar to conveniently
build, flash, debug, and run programs for the STM32F4 board.

The following is required:
 - a recent GNAT compiler targeting arm-elf
 - (optional, required for running programs on the board)
   the utility stlink present on the PATH.
   This is included in recent versions of GNAT, or can be downloaded at
     https://github.com/texane/stlink

 - (optional, required for running programs in the emulator)
   the arm-eabi-gnatemu emulator should be present on the PATH
"""

import GPS
from modules import Module
import gps_utils.workflow as workflow
from gps_utils.workflow import WORKFLOW_PARAMETER
import gps_utils.promises as promise
import sys


def msg_is(msg):
    GPS.Console("Messages").write(msg + "\n")


class BoardLoader(Module):

    # a list of targets
    __targets = ["Flash to Board",
                 "Debug on Board",
                 "Run with Emulator",
                 "Debug with Emulator"]
    __buttons = []

    def __error_exit(self, msg="", reset_refresh=False, reset_loading=False):
        """ Emit an error and reset the workflows """
        GPS.Console("Messages").write(msg + " [workflow stopped]")
        self.__refresh = reset_refresh
        self.__is_loading = reset_loading

    def __reset_all(self, id, manager_delete=True, connection_delete=True):
        """ Reset the workflows """
        if self.__manager is not None and manager_delete:
            self.__manager.get().non_blocking_send("q")
            self.__manager = None
        if self.__connection is not None and connection_delete:
            self.__connection.get().kill()
            self.__connection = None
        interest = ["st-util", "arm-eabi-gnatemu"][id]
        for i in GPS.Task.list():
            if interest in i.name():
                i.interrupt()
        self.__refresh = False
        self.__is_loading = False

    def __check_task(self, id):
        """ Back up method to check if task exists
        """
        r = False
        interest = ["st-util", "arm-eabi-gnatemu"][id]
        for i in GPS.Task.list():
            if interest in i.name():
                r = True
        return r

    def __show_button(self):
        """Initialize buttons and parameters.
        """
        # make loading a critical region
        self.__is_loading, self.__refresh = False, False

        # show button if the following criteria are met:
        # the string "stm32f3" should be found in the runtime;
        # we also look in the "legacy" way of specifying this,
        # in the --RTS switches
        p = GPS.Project.root()
        s = p.get_attribute_as_string(package="Builder",
                                      attribute="Default_Switches",
                                      index="Ada") + \
            p.get_attribute_as_string(package="Builder",
                                      attribute="Switches",
                                      index="Ada") + \
            p.get_attribute_as_string("runtime", index="Ada")

        if "stm32f4" in s:
            for b in self.__buttons:
                b.show()
        else:
            for b in self.__buttons:
                b.hide()

        # reset
        self.__manager, self.__connection = None, None

    ###############################
    # The following are workflows #
    ###############################

    def __flash_wf(self):
        """Workflow to build and flash the program on the board.
        """

        self.__is_loading = True

        # STEP 1.0 get the main name; it should have been passed to the driver
        f = yield WORKFLOW_PARAMETER
        if f is None:
            self.__error_exit(msg="Could not find the name of the main.")
            return

        # STEP 1.5 Build it
        msg_is("Building Main %s..." % f)
        builder = promise.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(f)
        if r0 is not 0:
            self.__error_exit(msg="... Build error.")
            return

        msg_is("... done.")

        # STEP 2 create executable
        msg_is("Creating the binary executable...")
        b = GPS.Project.root().get_executable_name(GPS.File(f))
        d = GPS.Project.root().object_dirs()[0]
        obj = d+b
        binary = obj+".bin"
        cmd = ["arm-eabi-objcopy", "-O", "binary", obj, binary]
        try:
            con = promise.ProcessWrapper(cmd)
        except:
            self.__error_exit("Could not launch executable st-flash.")
            return

        r1 = yield con.wait_until_terminate()
        if r1 is not 0:
            self.__error_exit("arm-eabi-objcopy returned an error.")
            return

        msg_is("... done.")

        # STEP 3.1 connect to mainboard
        msg_is("Connecting to board...")
        cmd = ["st-flash", "write", binary, "0x8000000"]
        try:
            con = promise.ProcessWrapper(cmd)
        except:
            self.__error_exit("Could not connect to the board.")
            return

        r2 = yield con.wait_until_match(
            "Starting verification of write complete",
            15000)
        r3 = yield con.wait_until_match(
            "Flash written and verified! jolly good!",
            500)

        if not (r2 and r3):
            self.__error_exit(msg="Could not flash the executable.")
            con.get().kill()
            return

        msg_is("... done.")
        msg_is("Running on board...")

        self.__is_loading = False

    def __emu_wf(self):
        """
        Workflow to build and run the program in the emulator.
        """

        self.__is_loading = True

        # STEP 1.0 get main name
        f = yield WORKFLOW_PARAMETER
        if f is None:
            self.__error_exit(msg="Main not specified")
            return

        # STEP 1.5 Build it
        msg_is("Building main %s..." % f)
        builder = promise.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(f)
        if r0 is not 0:
            self.__error_exit(msg="Build error.")
            return

        msg_is("... done.")

        # STEP 2 load with Emulator
        msg_is("Initializing emulator...")

        b = GPS.Project.root().get_executable_name(GPS.File(f))
        d = GPS.Project.root().object_dirs()[0]
        obj = d + b
        cmd = ["arm-eabi-gnatemu", "--board=STM32F4", obj]
        try:
            self.__connection = promise.ProcessWrapper(cmd)
        except:
            self.__error_exit("Executable arm-eabi-gnatemu not installed.")
            return

        msg_is("... done.")

        msg_is("Running in emulator...")

        self.__is_loading = False

    def __emu_debug_wf(self):
        """
        Workflow to debug a program under the emulator.
        """
        # check if there's a debugger running, and if so, interrupt it
        if self.__manager is not None:
            try:
                GPS.execute_action("/Debug/Interrupt")
            except:
                self.__refresh = False
                self.__manager = None
                pass
        else:
            # if there is not a debugger running, reset the parameters
            self.__refresh = False

        # STEP 1.0 get main name
        f = yield WORKFLOW_PARAMETER
        if f is None:
            self.__error_exit(msg="Main not specified.")
            return

        # STEP 1.5 Build it
        msg_is("Building Main %s..." % f)
        builder = promise.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(f)
        if r0 is not 0:
            self.__error_exit(msg="Build error.")
            return

        msg_is("... done.")

        # STEP 2 load with Emulator
        msg_is("Initializing emulator...")
        b = GPS.Project.root().get_executable_name(GPS.File(f))
        d = GPS.Project.root().object_dirs()[0]
        obj = d + b
        cmd = ["arm-eabi-gnatemu", "-g", "--board=STM32F4", obj]
        try:
            self.__connection = promise.ProcessWrapper(cmd)
        except:
            msg_is("Executable arm-eabi-gnatemu not installed.")
            return

        msg_is("... done.")

        # STEP 3.1 launch debugger

        if not self.__refresh:
            if self.__manager is None:
                msg_is("Initializing debugger...")
                self.__manager = promise.DebuggerWrapper(GPS.File(b))
            # block execution until debugger is not busy
            r3 = yield self.__manager.wait_and_send(cmd="", block=False)
            if not r3:
                self.__error_exit("Could not initialize the debugger.")
                r3 = yield self.__manager.wait_and_send(cmd="", block=False)
                self.__reset_all  # ??? is this the right reset?
                return
            msg_is("... done.")

        # STEP 3.2 target and run the program
        msg_is("Sending debugger command to target the emulator...")
        r3 = yield self.__manager.wait_and_send(
            cmd="target remote localhost:1234",
            timeout=4000)
        interest = "Remote debugging using localhost:1234"

        if interest not in r3:
            self.__error_exit("Could not connect to the target.")
            self.__reset_all(1)
            return

        msg_is("... done.")

        # self.__manager.get().non_blocking_send("c")

        self.__is_loading = False
        self.__refresh = True

    def __debug_wf(self):
        """
        Workflow to build, flash and debug the program on the real board.
        """
        self.__is_loading = True

        # check if there's a debugger running, and if so, interrupt it
        if self.__manager is not None:
            try:
                GPS.Debugger.get()
                GPS.execute_action("/Debug/Interrupt")
                GPS.Console("Messages").write(
                    "\nRunning Debugger Interrupted.")
            except:
                self.__refresh = False
                self.__manager = None
                pass
        else:
            # if there is not a debugger running, reset the parameters
            self.__refresh = False

        # STEP 1.0 get main name

        f = yield WORKFLOW_PARAMETER
        if f is None:
            self.__error_exit(msg="Main not specified")
            return

        # STEP 1.5 Build it

        msg_is("Building Main %s..." % f)
        builder = promise.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(f)
        if r0 is not 0:
            self.__error_exit("Build error.")
            return

        msg_is("... done.")

        # STEP 2 connect to mainboard

        if not self.__refresh:
            msg_is("Connecting to board...")
            cmd = ["st-util"]

            try:
                con = promise.ProcessWrapper(cmd)
            except:
                self.__error_exit("Could not launch st-util.")
                return

            self.__connection = con
            msg_is("... done.")

        # STEP 3 begin debugger-> load and run
        msg_is("Loading executable file...")

        b = GPS.Project.root().get_executable_name(GPS.File(f))
        d = GPS.Project.root().object_dirs()[0]
        obj = d+b
        # if __refresh is True, load the newly compiled obj
        # else start a debugger with the obj
        if self.__refresh:
            m1 = GPS.Console("Debugger Console").get_text()
            self.__manager.get().non_blocking_send("load "+obj)
            r3 = yield self.__manager.wait_and_send(cmd="", block=True)
            m2 = GPS.Console("Debugger Console").get_text()
            if len(m2) >= len(m1):
                r3 = not("Error" in m2[len(m1)::])
        else:
            self.__manager = promise.DebuggerWrapper(GPS.File(b))
            # block execution until debugger is not busy
            r3 = yield self.__manager.wait_and_send(cmd="", block=True)

        if not r3:
            self.__error_exit("Connection Lost. "
                              + "Please check the USB connection and restart.")
            r3 = yield self.__manager.wait_and_send(cmd="", block=True)
            self.__reset_all(0)
            return

        msg_is("... done.")

        # STEP 3.5 run the program and set __refresh with True
        self.__refresh = True
        self.__is_loading = False

    # The followings are hooks:

    def gps_started(self):
        """
        When GPS start, add button (include cireteria there)
        """
        # add hooks
        GPS.Hook("debugger_terminated").add(self.debugger_terminated)

        # Create targets * 4:
        workflow.create_target_from_workflow("Flash to Board",
                                             "flash-to-board",
                                             self.__flash_wf,
                                             "gps-boardloading")
        workflow.create_target_from_workflow("Debug on Board",
                                             "debug-on-board",
                                             self.__debug_wf,
                                             "gps-boardloading-debug")
        workflow.create_target_from_workflow("Run with Emulator",
                                             "run-with-emulator",
                                             self.__emu_wf,
                                             "gps-emulatorloading")
        workflow.create_target_from_workflow("Debug with Emulator",
                                             "debug-with-emulator",
                                             self.__emu_debug_wf,
                                             "gps-emulatorloading-debug")

        for i in range(0, 4):
            b = GPS.BuildTarget(self.__targets[i])
            self.__buttons.append(b)

        self.__show_button()

    def project_view_changed(self):
        """
        When project view changes, add button (include cireteria there)
        """
        self.__show_button()

    def debugger_terminated(self, hookname, debugger):
        """
        When debugger terminates, kill connection.
        """
        self.__reset_all(id=1)
        self.__reset_all(id=0)
