"""
This plug-in is to create a button on the toolbar
that triggers compile, load, and run for ada projects
onto a bare STM32F4 board.

        **Important**FOR SAFETY**
    The executable program loaded by clicking this button
    are those designed for STM32F4 particular bareboard.
    Other models untested.

For this commit:
- Successful usage requires third-party utility stlink
  https://github.com/texane/stlink
  * To try other utility, please change
    cmd = ["st-util"] -> cmd = ["the", "one", "you", "have"]
    on line 74, and restart GPS.
"""

import GPS
from modules import Module
import gps_utils.workflow as workflow
import gps_utils.promises as promise


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
            # __manager is debugger wrapper instance
            self.__manager = None

            # __process is process wrapper instance
            self.__process = None

            # indicates a refresh on previously running debugger
            self.__refresh = False

            self.__button = GPS.Button("load-on-bareboard",
                                       "Load On Board",
                                       self.__load)
            GPS.Toolbar().insert(self.__button, 0)

    def __loading_workflow(self):
        """
           Generator: create workflow for loading
        """
        # STEP 1 add hook to compiler, and compile the program
        builder = promise.TargetWrapper("Build All")
        r0 = yield builder.wait_on_execute()
        if r0 is not 0:
            GPS.Console("Messages").write(
                "Compilation Error.\nExit Board Loading.")
            self.__refresh = False
            return

        # STEP 2 connect to mainboard
        # if __refresh is True: not need to do so
        if not self.__refresh:
            cmd = ["st-util"]
            try:
                con = promise.ProcessWrapper(cmd)
            except:
                GPS.Console("Messages").write(
                    "Conection Tools Missing.\nExit Board Loading.")
                self.__refresh = False
                return
            r1 = yield con.wait_until_match("Device connected is")
            r2 = yield con.wait_until_match("Listening at")
            if not (r1 and r2):
                GPS.Console("Messages").write(
                    "Connection Error.\nExit Board Loading.")
                self.__refresh = False
                return
            self.__process = con.get()

        # STEP 3 begin debugger-> load and run
        f = GPS.Project.root().get_attribute_as_list("main")[0]
        b = GPS.Project.root().get_executable_name(GPS.File(f))
        d = GPS.Project.root().object_dirs()[0]
        obj = d+b
        # if __refresh is True, load the newly compiled obj
        if self.__refresh:
            r3 = yield self.__manager.wait_and_send(cmd="load "+obj)
        # else start a debugger with the obj
        else:
            self.__manager = promise.DebuggerWrapper(GPS.File(b))

        # block execution until debugger is not busy
        r4 = yield self.__manager.wait_and_send(cmd="", block=True)
        if not r4:
            GPS.Console("Messages").write(
                "Fail Loading/Running.\nExit.")
            self.__refresh = False
            return
        # STEP 3.5 run the program and set __refresh with True
        self.__manager.get().non_blocking_send("c")
        self.__refresh = True

    def __load(self, button):
        """
           A trigger. Called by GPS when button is clicked
        """
        # check if there's a debugger running, and if so, interrupt it
        try:
            db = GPS.Debugger.get()
            GPS.execute_action("/Debug/Interrupt")
        except:
            # if there is not a debugger running, reset the parameters
            self.__refresh = False
            self.__manager = None

        # create the workflow from generator and run it
        w = self.__loading_workflow()
        workflow.driver(w)

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
