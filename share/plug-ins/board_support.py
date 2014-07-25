"""
This plug-in is to create a button on the toolbar
that triggers compile, load, and run for ada projects
onto a bare STM32F4 board.

             **Important**FOR SAFETY**
    The executable programs that can be loaded
    by clicking this button are those designed
    for STM32F4 particular bareboard.
               Other models untested.

For successful usage:
- third-party utility stlink required:
  https://github.com/texane/stlink
- if using other tool to connect board, try replace st-util
  with your util in the content.
"""

import GPS
from modules import Module
import gps_utils.workflow as workflow
import gps_utils.promises as promise
import sys


class BoardLoader(Module):
    """
       This class defines a manager of the button which call:
           compiles, connect and load
       of programs onto a board.
       One instance, as a global manager that takes care of button-execution,
       will be created everytime project of certain type is loaded by GPS.
    """
    __button = None

    def __add_button(self):
        """
           Add_button when criteria meets.
           Initialize parameters.

           criteria = the program is written and can be built for
           board stm32f4.
        """
        # __manager is debugger wrapper instance
        self.__manager = None

        # __connection is process wrapper instance
        self.__connection = None

        # indicates a refresh and reconnect on previously running debugger
        self.__refresh = False
        # self.__reconnect = True

        # make loading a critical region
        self.__is_loading = False

        # destroy the button if it exists
        if self.__button is not None:
            self.__button.destroy()
            self.__button = None

        # create a button and add it to the toolbar
        # if the following criteria meets:
        p = GPS.Project.root()
        s = p.get_attribute_as_string(package="Builder",
                                      attribute="Default_Switches",
                                      index="Ada") + \
            p.get_attribute_as_string(package="Builder",
                                      attribute="Switches",
                                      index="Ada")
        if "stm32f4" in s:
            self.__button = GPS.Button("load-on-bareboard",
                                       "Load On Board",
                                       self.__load)
            GPS.Toolbar().insert(self.__button, 0)

    def __loading_workflow(self):
        """
           Generator: create workflow for loading
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

        # STEP 1 add hook to compiler, and compile the program
        GPS.Console("Messages").write(
            "\nBoard_Loader_STEP: Building Main...")

        builder = promise.TargetWrapper("Build All")

        r0 = yield builder.wait_on_execute()
        if r0 is not 0:
            self.__error_exit("Compilation Error.\nExit Board Loading.\n")
            return

        GPS.Console("Messages").write(
            "Build Complete!\n")

        # STEP 2 connect to mainboard

        if not self.__refresh:
            GPS.Console("Messages").write(
                "\nBoard_Loader_STEP: Connecting to board...")
            cmd = ["st-util"]

            try:
                con = promise.ProcessWrapper(cmd)
            except:
                self.__error_exit("Can't call stlink. Exit Board Loading.\n")
                return

            r1 = yield con.wait_until_match("Device connected is", 2000)
            r2 = yield con.wait_until_match("Listening at", 500)

            if not (r1 and r2):
                self.__error_exit("Connection Error. Exit Board Loading.\n")
                con.get().kill()
                return

            self.__connection = con
            GPS.Console("Messages").write("Complete!\n")

        # STEP 3 begin debugger-> load and run
        GPS.Console("Messages").write(
            "\nBoard_Loader_STEP: Loading executable file...")

        f = GPS.Project.root().get_attribute_as_list("main")[0]
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
            self.__reset_all()
            return

        GPS.Console("Messages").write("Complete!\n")

        # STEP 3.5 run the program and set __refresh with True
        GPS.Console("Messages").write("\nBoard_Loader_Complete!\n")
        self.__manager.get().non_blocking_send("c")
        self.__refresh = True
        self.__is_loading = False

    def __load(self, button):
        """
           A trigger. Called by GPS when button is clicked
        """
        # 1 check if I'm loading a workflow already, if so, exit
        if self.__is_loading:
            return

        # 2 verify connections
        if self.__refresh and (not self.__check_task()):
            self.__error_exit("Connection Lost. "
                              + "Please ensure USB connection and restart. "
                              + "Exit.\n")
            self.__reset_all()
            return

        # create the workflow from generator and run it
        w = self.__loading_workflow()
        workflow.driver(w)

    def __error_exit(self, msg="", reset_refresh=False, reset_loading=False):
        GPS.Console("Messages").write(msg)
        self.__refresh = reset_refresh
        self.__is_loading = reset_loading

    def __reset_all(self):
        if self.__manager is not None:
            self.__manager.get().non_blocking_send("q")
            self.__manager = None
        self.__connection.get().kill()
        for i in GPS.Task.list():
            if "st-util" in i.name():
                i.interrupt()
        self.__connection = None
        self.__refresh = False
        self.__is_loading = False

    def __check_task(self):
        r = False
        for i in GPS.Task.list():
            if "st-util" in i.name():
                r = True
        return r

    # The followings are hooks:

    def gps_started(self):
        """
           When GPS start, add button (include cireteria there)
        """
        self.__add_button()

    def project_view_changed(self):
        """
           When project view changes, add button (include cireteria there)
        """
        self.__add_button()

    def project_changed(self):
        """
           When project changes, add button (include cireteria there)
        """
        self.__add_button()
