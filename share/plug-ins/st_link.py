"""
This plug-in creates buttons on the toolbar to conveniently
build, flash, debug, and run programs for the STM32F4 board.

The following is required:
 - a recent GNAT compiler targeting arm-elf
 - (optional, required for running programs on the board)
   the utility stlink present on the PATH.
   This is included in recent versions of GNAT, or can be downloaded at
     https://github.com/texane/stlink

"""

import GPS
from modules import Module
import gps_utils.workflow as workflow
from gps_utils.workflow import WORKFLOW_PARAMETER
import gps_utils.promises as promise


def msg_is(msg):
    GPS.Console("Messages").write(msg + "\n")


class BoardLoader(Module):

    # a list of targets
    __targets = ["Flash to Board",
                 "Debug on Board"]
    __buttons = []
    __connection = None

    def __error_exit(self, msg=""):
        """ Emit an error and reset the workflows """
        GPS.Console("Messages").write(msg + " [workflow stopped]")

    def __reset_all(self, manager_delete=True, connection_delete=True):
        """ Reset the workflows """
        if self.__connection is not None and connection_delete:
            self.__connection.get().kill()
            self.__connection = None
        interest = "st-util"
        for i in GPS.Task.list():
            if interest in i.name():
                i.interrupt()

    def __check_task(self, id):
        """ Back up method to check if task exists
        """
        r = False
        interest = ["st-util"][id]
        for i in GPS.Task.list():
            if interest in i.name():
                r = True
        return r

    def __show_button(self):
        """Initialize buttons and parameters.
        """
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
        self.__connection = None

    ###############################
    # The following are workflows #
    ###############################

    def __flash_wf(self):
        """Workflow to build and flash the program on the board.
        """

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

    def __debug_wf(self):
        """
        Workflow to build, flash and debug the program on the real board.
        """
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

        debugger_promise = promise.DebuggerWrapper(GPS.File(b))
        debugger_promise.get().non_blocking_send("load "+obj)
        r3 = yield debugger_promise.wait_and_send(cmd="", block=True)

        if not r3:
            self.__error_exit("Connection Lost. "
                              + "Please check the USB connection and restart.")
            r3 = yield debugger_promise.wait_and_send(cmd="", block=True)
            self.__reset_all()
            return

        msg_is("... done.")

        # STEP 3.5 run the program

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

        for tar in self.__targets:
            b = GPS.BuildTarget(tar)
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
        self.__reset_all()
