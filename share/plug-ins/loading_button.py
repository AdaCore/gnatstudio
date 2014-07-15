"""
This plug-in is to create a button on the toolbar
that triggers compile, load, and run for ada projects
onto a bare STM32F4 board.
"""

import GPS
import gps_utils.workflow as workflow
import gps_utils.promises as promise


class BoardLoader():
    """
       This class defines managers for the button which call:
           compiles, connect and load
       of programs onto a board.
       One instance, as a global manager that takes care of button-execution,
       will be created everytime GPS starts.
    """
    def __init__(self):
        self.__manager = None
        self.__process = None
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
            print "Compilation Error.\nExiting..."
            return

        # STEP 2 connect to mainboard
        cmd = ["/home/qh/Documents/AdaCore/hardware/stlink/st-util"]
        con = promise.ProcessWrapper(cmd)
        r1 = yield con.wait_until_match("Device connected is")
        r2 = yield con.wait_until_match("Listening at")
        if not (r1 and r2):
            print "Connection Error.\nExiting..."
            return
        self.__process = con.get()

        # STEP 3 begin debugger
        f = GPS.Project.root().get_attribute_as_list("main")[0]
        b = GPS.Project.root().get_executable_name(GPS.File(f))
        db = promise.DebuggerWrapper(GPS.File(b))
        r3 = yield db.wait_and_send(cmd="", block=True)
        if not r3:
            print "Loading Error.\nExiting..."
            return
        self.__refresh = True
        self.__manager = db.get()
        self.__manager.non_blocking_send("c")

    def __load(self, button):
        """
           Called by GPS when button is clicked
        """
        # check if there's debugger running, and if so, terminate it
        if self.__refresh:
            self.__manager.non_blocking_send("q")
            self.__process.kill()
            self.refresh = False

        # create the denerator and run it
        w = self.__loading_workflow()
        workflow.driver(w)

bl = BoardLoader()
