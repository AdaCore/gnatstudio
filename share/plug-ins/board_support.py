"""
This plug-in is to create buttons on the toolbar
that triggers compile, load, and run for ada projects
onto a bare STM32F4 board or an emulator.

             **Important**FOR SAFETY**
    The executable programs that can be loaded
    by clicking this button are those designed
    for STM32F4 particular bareboard.
               Other models untested.

For successful usage:
- third-party utility stlink required:
  https://github.com/texane/stlink
  OR
  you could have the emulator: arm-eabi-gnatemu installed

- if using other tool to connect board, try replace st-util
  with your util in the content.
"""

import GPS
from modules import Module
import gps_utils.workflow as workflow
import gps_utils.promises as promise
import sys


def msg_is(msg):
    GPS.Console("Messages").write(msg)


class BoardLoader(Module):

    __targets = ["Flash to Board",
                 "Debug on Board",
                 "Run with Emulator",
                 "Debug with Emulator"]
    __buttons = []

    def __error_exit(self, msg="", reset_refresh=False, reset_loading=False):
        GPS.Console("Messages").write(msg)
        self.__refresh = reset_refresh
        self.__is_loading = reset_loading

    def __reset_all(self, id, manager_delete=True, connection_delete=True):
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
        """
        Back up method to check if task exists
        """
        r = False
        interest = ["st-util", "arm-eabi-gnatemu"][id]
        for i in GPS.Task.list():
            if interest in i.name():
                r = True
        return r

    def __show_button(self):
        """
           Show buttons when criteria meets.
           Initialize parameters.

           criteria = the program is written and can be built for
           board stm32f4.
        """
        # make loading a critical region
        self.__is_loading, self.__refresh = False, False

        # show button if the following criteria meets:
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

    # The followings are workflows/generators
    def __flash_wf(self):
        """
        BUILD FLASH program to REAL BOARD -- Yes, I'm a workflow.
        """

        self.__is_loading = True

        # STEP 1.0 get main name
        f = yield "give_me_arg"
        print "f-main is:", f
        if f is None:
            self.__error_exit(msg="Main name ?")
            return

        # STEP 1.5 Build it
        msg_is("\nBoard_Loader_STEP: Building Main...")
        builder = promise.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(f)
        print "r0 is:", r0
        if r0 is not 0:
            self.__error_exit(msg="Compilation Error.\nExit.\n")
            return

        msg_is("Build Complete!\n")

        # STEP 2 ma"give_me_arg"xecutable
        msg_is("\nBoard_Loader_STEP: Creating binary executable...")
        b = GPS.Project.root().get_executable_name(GPS.File(f))
        d = GPS.Project.root().object_dirs()[0]
        obj = d+b
        binary = obj+".bin"
        cmd = ["arm-eabi-objcopy", "-O", "binary", obj, binary]
        try:
            con = promise.ProcessWrapper(cmd)
        except:
            self.__error_exit("st-flash not installed/callable. Exit.\n")
            return

        r1 = yield con.wait_until_terminate()
        if r1 is not 0:
            self.__error_exit("arm-eabi-objcopy Error. Exit.\n")
            return

        msg_is("Complete!\n")

        # STEP 3.1 connect to mainboard
        msg_is("\nBoard_Loader_STEP: Connecting to board...")
        cmd = ["st-flash", "write", binary, "0x8000000"]
        try:
            con = promise.ProcessWrapper(cmd)
        except:
            msg_is("Fail to connect. Exit.\n")
            return

        r2 = yield con.wait_until_match(
            "Starting verification of write complete",
            15000)
        r3 = yield con.wait_until_match(
            "Flash written and verified! jolly good!",
            500)

        if not (r2 and r3):
            self.__error_exit(msg="Loading Error. Exit.\n")
            con.get().kill()
            return

        msg_is("Complete!\n")

        msg_is("\nRunning on board...")

        self.__is_loading = False

    def __emu_wf(self):
        """
        BUILD FLASH program with EMULATOR -- Yes, I'm a workflow.
        """

        self.__is_loading = True

        # STEP 1.0 get main name
        f = yield "give_me_arg"
        if f is None:
            self.__error_exit(msg="Main name ?")
            return

        # STEP 1.5 Build it
        msg_is("\nEmulator_STEP: Building Main...")
        builder = promise.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(f)
        if r0 is not 0:
            self.__error_exit(msg="Compilation Error.\nExit Emulator.\n")
            return

        msg_is("Build Complete!\n")

        # STEP 2 load with Emulator
        msg_is("\nEmulator_STEP: Initialize emulator...")

        b = GPS.Project.root().get_executable_name(GPS.File(f))
        d = GPS.Project.root().object_dirs()[0]
        obj = d + b
        cmd = ["arm-eabi-gnatemu", "--board=STM32F4", obj]
        try:
            self.__connection = promise.ProcessWrapper(cmd)
        except:
            msg_is("Emulator: arm-eabi-gnatemu not installed. Exit.\n")
            return

        msg_is("Complete!\n")

        msg_is("\nRunning with emulator...")

        self.__is_loading = False

    def __emu_debug_wf(self):
        """
        BUILD FLASH program with EMULATOR and DEBUGGER -- Yes, I'm a workflow.
        """
        # check if there's a debugger running, and if so, interrupt it
        if self.__manager is not None:
            try:
                GPS.Debugger.get()
                GPS.execute_action("/Debug/Interrupt")
                GPS.Console("Messages").write(
                    "\nRunning Debugger Interrupted.\n")
            except:
                self.__refresh = False
                self.__manager = None
                pass
        else:
            # if there is not a debugger running, reset the parameters
            self.__refresh = False

        # STEP 1.0 get main name
        f = yield "give_me_arg"
        if f is None:
            self.__error_exit(msg="Main name ?")
            return

        # STEP 1.5 Build it
        msg_is("\nEmulator_STEP: Building Main...")
        builder = promise.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(f)
        if r0 is not 0:
            self.__error_exit(msg="Compilation Error.\nExit Emulator.\n")
            return

        msg_is("Complete!\n")

        # STEP 2 load with Emulator
        msg_is("\nEmulator_STEP: Initialize emulator...")
        b = GPS.Project.root().get_executable_name(GPS.File(f))
        d = GPS.Project.root().object_dirs()[0]
        obj = d + b
        cmd = ["arm-eabi-gnatemu", "-g", "--board=STM32F4", obj]
        try:
            self.__connection = promise.ProcessWrapper(cmd)
        except:
            msg_is("Emulator: arm-eabi-gnatemu not installed. Exit.\n")
            return

        msg_is("Complete!\n")

        # STEP 3.1 launch debugger

        if not self.__refresh:
            msg_is("Emulator_STEP: initializing debugger...")
            self.__manager = promise.DebuggerWrapper(GPS.File(b))
            # block execution until debugger is not busy
            r3 = yield self.__manager.wait_and_send(cmd="", block=True)
            if not r3:
                self.__error_exit("Debugger has error. Exit.\n")
                r3 = yield self.__manager.wait_and_send(cmd="", block=True)
                self.__reset_all
                return
            msg_is("Complete!\n")

        # STEP 3.2 target and run the program
        msg_is("Emulator_STEP: targeting to remote localhost...")
        r3 = yield self.__manager.wait_and_send(
            cmd="target remote localhost:1234",
            timeout=4000)
        interest = "Remote debugging using localhost:1234"

        if interest not in r3:
            self.__error_exit("Fail to get target. Exit.\n")
            self.__reset_all(1)
            return

        msg_is("Complete!\n")

        # self.__manager.get().non_blocking_send("c")

        self.__is_loading = False
        self.__refresh = True

    def __debug_wf(self):
        """
        BUILD FLASH program with REALBOARD and DEBUGGER -- Yes, I'm a workflow.
        """
        self.__is_loading = True

        # check if there's a debugger running, and if so, interrupt it
        if self.__manager is not None:
            try:
                GPS.Debugger.get()
                GPS.execute_action("/Debug/Interrupt")
                GPS.Console("Messages").write(
                    "\nRunning Debugger Interrupted.\n")
            except:
                self.__refresh = False
                self.__manager = None
                pass
        else:
            # if there is not a debugger running, reset the parameters
            self.__refresh = False

        # STEP 1.0 get main name
        f = yield "give_me_arg"
        if f is None:
            self.__error_exit(msg="Main name ?")
            return

        # STEP 1.5 Build it
        msg_is("\nBoard_Loader_STEP: Building Main...")
        builder = promise.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(f)
        if r0 is not 0:
            self.__error_exit("Compilation Error. Exit.\n")
            return

        msg_is("Build Complete!\n")

        # STEP 2 connect to mainboard

        if not self.__refresh:
            msg_is("Board_Loader_STEP: Connecting to board...")
            cmd = ["st-util"]

            try:
                con = promise.ProcessWrapper(cmd)
            except:
                self.__error_exit("st-util not installed/callable.\n")
                return

            self.__connection = con
            GPS.Console("Messages").write("Complete!\n")

        # STEP 3 begin debugger-> load and run
        msg_is("Board_Loader_STEP: Loading executable file...")

        b = GPS.Project.root().get_executable_name(GPS.File(f))
        d = GPS.Project.root().object_dirs()[0]
        obj = d+b
        # if __refresh is True, load the newly compiled obj
        # else start a debugger with the obj
        if self.__refresh:
            m1 = GPS.Console("Debugger Console").get_text()
            self.__manager.get().non_blocking_send("load"+obj)
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
                              + "Please ensure USB connection and restart. "
                              + "Exit.\n")
            r3 = yield self.__manager.wait_and_send(cmd="", block=True)
            self.__reset_all(0)
            return

        msg_is("Complete!\n")

        # STEP 3.5 run the program and set __refresh with True
        msg_is("\nBoard_Loader_Complete!\n")
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
